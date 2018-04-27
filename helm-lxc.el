;;; -*- lexical-binding: t -*-

(require 'tramp)
(require 's)
(eval-when-compile
  (require 'cl-lib))

(defgroup helm-lxc nil
  "Helm interface to manage LXC containers."
  :group 'helm)

(defface helm-lxc-face-running
  '((t . (:foreground "green")))
  "Face use to colorize the names of running containers."
  :group 'helm-lxc)

(defface helm-lxc-face-stopped
  '((t . (:foreground "red")))
  "Face use to colorize the names of stopped containers."
  :group 'helm-lxc)

(defface helm-lxc-face-frozen
  '((t . (:foreground "blue")))
  "Face use to colorize the names of frozen containers."
  :group 'helm-lxc)

(defcustom helm-lxc-hosts '(("localhost" . "/sudo::"))
  "Alist of hosts to check for containers.
Each member of the alist is of the form (NAME . TRAMP-PATH).
TRAMP-PATH specify where to get information about
containers. NAME is the name of the entry and is used for display
purpose. If you use nil as TRAMP-PATH for an entry of the alist,
all the commands for this entry will be run on the local machine
as the user running Emacs."
  :group 'helm-lxc
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Tramp file name")))

(defcustom helm-lxc-delete-window-on-exit t
  "Delete the window when the attached shell exits."
  :group 'helm-lxc
  :type 'boolean)

(defcustom helm-lxc-attach-with-ssh nil
  "Attach to the container using SSH instead of lxc-attach.
If nil, SSH will never be used to attach to the container. If
non-nil, SSH will be used if the container has at least an IP
address (the first one returned by lxc-info is used)."
  :group 'helm-lxc
  :type 'boolean)

(defcustom helm-lxc-attach-ssh-user "root"
  "User used to connect to container when `helm-lxc-attach-with-ssh is non-nil."
  :group 'helm-lxc
  :type 'string)

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
    (let ((default-directory (or host default-directory)))
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
  (let* ((state (downcase (car (helm-lxc--process-lines
                                host
                                "lxc-info"
                                nil
                                "-s" "-H" "-n" container))))
         (is-stopped (string-equal state "stopped"))
         (pid (and (not is-stopped)
                   (car (helm-lxc--process-lines
                         host
                         "lxc-info"
                         nil
                         "-p" "-H" "-n" container))))
         (ips (and (not is-stopped)
                   (split-string
                    (car (helm-lxc--process-lines
                          host
                          "lxc-info"
                          nil
                          "-i" "-H" "-n" container))
                    ","))))
    `((host . ,host)
      (name . ,container)
      (state . ,state)
      (pid . ,pid)
      (ips . ,ips))))

(defun helm-lxc--attach (container)
  (let* ((source (assoc-default 'name (helm-get-current-source)))
         (name (alist-get 'name container))
         (host (alist-get 'host container))
         (ip (car (alist-get 'ips container)))
         (use-ssh (and helm-lxc-attach-with-ssh ip))
         (default-directory (or (and use-ssh
                                     (format "%s|ssh:%s@%s:"
                                             (if (file-remote-p host)
                                                 (substring host 0 -1)
                                               host)
                                             helm-lxc-attach-ssh-user
                                             ip))
                                host))
         (shell-file-name "/bin/bash")
         (explicit-bash-args (or (and (not use-ssh)
                                      `("-c" ,(format
                                               "lxc-attach -n %s -- bash %s"
                                               name
                                               (s-join " " explicit-bash-args))))
                                 explicit-bash-args))
         (buffer (shell (format "*shell %s@%s*" name source)))
         (proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      (unless use-ssh
        ;; remove echoed character
        (setq comint-process-echoes t)))
    (when helm-lxc-delete-window-on-exit
      (add-function :after (process-sentinel proc) #'helm-lxc--process-sentinel))))

(defun helm-lxc--execute-action (action)
  (lambda (_)
    (dolist (container (helm-marked-candidates :all-sources))
      (let* ((name (alist-get 'name container))
             (host (alist-get 'host container))
             (default-directory (or host default-directory)))
        (unless (zerop (process-file (format "lxc-%s" action) nil nil nil "-n" name))
          (message "lxc-%s on %s failed" action name))))))

(defun helm-lxc--connect-to-host (container)
  (let* ((host (alist-get 'host container))
         (default-directory (or host default-directory))
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
      (insert (format "State: %s\n" (alist-get 'state container)))
      (insert (format "PID: %s\n" (alist-get 'pid container)))
      (insert (format "IPs: %s\n" (s-join " " (alist-get 'ips container)))))
    (switch-to-buffer buffer)))

(defun helm-lxc--action-transformer (actions container)
  (let ((connect-action '("Connect to host" . helm-lxc--connect-to-host)))
    (pcase (alist-get 'state container)
      (`"running" `(("Attach" . helm-lxc--attach)
                    ("Stop" . ,(helm-lxc--execute-action "stop"))
                    ("Freeze" . ,(helm-lxc--execute-action "freeze"))
                    ,connect-action))
      (`"stopped" `(("Start" . ,(helm-lxc--execute-action "start"))
                    ("Destroy" . ,(helm-lxc--execute-action "destroy"))
                    ,connect-action))
      (`"frozen" `(("Unfreeze" . ,(helm-lxc--execute-action "unfreeze"))
                   ,connect-action)))))

(defun helm-lxc--build-sources ()
  (cl-loop for (name . tramp-path) in helm-lxc-hosts
           collect
           (helm-build-sync-source name
             :candidates (helm-lxc--list-containers tramp-path)
             :action-transformer 'helm-lxc--action-transformer
             :persistent-action 'helm-lxc--show-container-info
             :persistent-help "Show container info")))

(defun helm-lxc ()
  (interactive)
  (helm
   :buffer "*helm lxc*"
   :sources (helm-lxc--build-sources)))
