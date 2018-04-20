;;; -*- lexical-binding: t -*-

(require 'tramp)
(eval-when-compile
  (require 'cl-lib))

(defgroup helm-lxc nil
  "Helm interface to manage LXC containers"
  :group 'helm)

(defface helm-lxc-face-running
  '((t . (:foreground "green")))
  "Face use to colorize the names of running containers"
  :group 'helm-lxc)

(defface helm-lxc-face-stopped
  '((t . (:foreground "red")))
  "Face use to colorize the names of stopped containers"
  :group 'helm-lxc)

(defface helm-lxc-face-frozen
  '((t . (:foreground "blue")))
  "Face use to colorize the names of frozen containers"
  :group 'helm-lxc)

(defcustom helm-lxc-hosts '(("localhost" . "/sudo::"))
  "Alist of hosts to check for containers"
  :group 'helm-lxc
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Tramp file name")))

(defun helm-lxc--face-from-state (state)
  (pcase state
    (`"running" 'helm-lxc-face-running)
    (`"stopped" 'helm-lxc-face-stopped)
    (`"frozen" 'helm-lxc-face-frozen)))

(defun helm-lxc--process-sentinel (proc _event)
  (unless (process-live-p proc)
    (let* ((buffer (process-buffer proc))
           (win (get-buffer-window buffer)))
      (when win
        (ignore-errors
          (delete-window win)))
      (kill-buffer buffer))))

(defun helm-lxc--process-lines (host program &optional delete-trailing-ws &rest args)
  (with-temp-buffer
    (let ((default-directory host))
      (when (zerop (apply 'process-file program nil t nil args))
        (when delete-trailing-ws
          (delete-trailing-whitespace))
        (goto-char (point-min))
        (let (lines)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (push line lines))
            (forward-line))
          lines)))))

(defun helm-lxc--list-containers (host)
  (let ((containers (helm-lxc--process-lines host "lxc-ls" t)))
    (nreverse
     (mapcar (lambda (c)
               (let* ((info (helm-lxc--get-container-info host c))
                      (state (alist-get 'state info))
                      (face (helm-lxc--face-from-state state)))
                 (cons (propertize c 'face face) info)))
             containers))))

(defun helm-lxc--get-container-info (host container)
  (let ((state (downcase (car (helm-lxc--process-lines
                               host
                               "lxc-info"
                               nil
                               "-s" "-H" "-n" container)))))
    `((host . ,host)
      (name . ,container)
      (state . ,state))))

(defun helm-lxc--attach (container)
  (let* ((name (alist-get 'name container))
         (host (alist-get 'host container))
         (default-directory host)
         (shell-file-name "/bin/bash")
         (explicit-bash-args `("-c" ,(format "lxc-attach -n %s -- bash --noediting -i" name)))
         (buffer (shell (format "*shell on %s@%s*" name host)))
         (proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      ;; remove echoed character
      (setq comint-process-echoes t))
    (add-function :after (process-sentinel proc) #'helm-lxc--process-sentinel)))

(defun helm-lxc--start (container)
  (let* ((name (alist-get 'name container))
         (host (alist-get 'host container))
         (default-directory host))
    (unless (zerop (process-file "lxc-start" nil nil nil "-n" name))
      (error "lxc-start on %s failed" name))))

(defun helm-lxc--stop (container)
  (let* ((name (alist-get 'name container))
         (host (alist-get 'host container))
         (default-directory host))
    (unless (zerop (process-file "lxc-stop" nil nil nil "-n" name))
      (error "lxc-stop on %s failed" name))))

(defun helm-lxc--connect-to-host (container)
  (let* ((host (alist-get 'host container))
         (default-directory host)
         (shell-file-name "/bin/bash")
         (buffer (shell (format "*shell on %s*" host)))
         (proc (get-buffer-process buffer)))
    (add-function :after (process-sentinel proc) #'helm-lxc--process-sentinel)))

(defun helm-lxc--show-container-info (container)
  (let ((buffer (get-buffer-create "*helm lxc info*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Name: %s\n" (alist-get 'name container)))
      (insert (format "Host: %s\n" (file-remote-p (alist-get 'host container) 'host)))
      (insert (format "State: %s\n" (alist-get 'state container))))
    (switch-to-buffer buffer)))

(defun helm-lxc--action-transformer (actions container)
  (let ((connect-action '("Connect to host" . helm-lxc--connect-to-host)))
    (pcase (alist-get 'state container)
      (`"running" `(("Attach" . helm-lxc--attach)
                    ("Stop" . helm-lxc--stop)
                    ,connect-action))
      (`"stopped" `(("Start" . helm-lxc--start)
                    ,connect-action))
      (`"frozen" `(("Unfreeze" . helm-lxc--unfreeze)
                   ,connect-action)))))

(defun helm-lxc--build-sources ()
  (cl-loop for (name . remote-path) in helm-lxc-hosts
           collect
           (helm-build-sync-source name
             :candidates (helm-lxc--list-containers remote-path)
             :action-transformer 'helm-lxc--action-transformer
             :persistent-action 'helm-lxc--show-container-info
             :persistent-help "Show container info")))

(defun helm-lxc ()
  (interactive)
  (helm
   :buffer "*helm lxc*"
   :sources (helm-lxc--build-sources)))
