;;; helm-lxc.el --- Helm interface to manage LXC containers -*- lexical-binding: t -*-

;; Copyright (C) 2018 montag451

;; Author: montag451
;; URL: https://github.com/montag451/helm-lxc
;; Keywords: helm, lxc, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (helm "2.9.4") (lxc-tramp "0.1.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'helm)
(require 'tramp)
(require 'lxc-tramp)
(require 'subr-x)
(eval-when-compile
  (require 'cl-lib))

(defgroup helm-lxc nil
  "Helm interface to manage LXC containers."
  :prefix "helm-lxc-"
  :link '(url-link :tag "GitHub" "https://github.com/montag451/helm-lxc")
  :group 'helm)

(defface helm-lxc-face-running
  '((t . (:foreground "green")))
  "Face use to colorize the name of running containers."
  :group 'helm-lxc)

(defface helm-lxc-face-stopped
  '((t . (:foreground "red")))
  "Face use to colorize the name of stopped containers."
  :group 'helm-lxc)

(defface helm-lxc-face-frozen
  '((t . (:foreground "blue")))
  "Face use to colorize the name of frozen containers."
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

(defcustom helm-lxc-clean-up-on-shell-exit nil
  "Do some cleanup when a shell exits.
If non-nil, when a shell (created by attaching to a container)
exits its buffer is killed and its window, if any, is quitted."
  :group 'helm-lxc
  :type 'boolean)

(defcustom helm-lxc-attach-with-ssh nil
  "Attach to the container using SSH instead of `lxc-tramp'.
If nil, SSH will never be used to attach to the container. If
non-nil, SSH will be used if the container has at least an IP
address (the first one returned by `lxc-info' is used)."
  :group 'helm-lxc
  :type 'boolean)

(defcustom helm-lxc-attach-ssh-user "root"
  "User used to connect to container when `helm-lxc-attach-with-ssh' is non-nil."
  :group 'helm-lxc
  :type 'string)

(defvar helm-lxc-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-s") #'helm-lxc-start-persistent)
    (define-key map (kbd "C-d") #'helm-lxc-stop-persistent)
    (define-key map (kbd "M-c") #'helm-lxc-clear-cache-persistent)
    map))

(defvar helm-lxc--cache (make-hash-table :test 'equal))

(defun helm-lxc--face-from-state (state)
  (pcase state
    ("running" 'helm-lxc-face-running)
    ("stopped" 'helm-lxc-face-stopped)
    ("frozen" 'helm-lxc-face-frozen)))

(defun helm-lxc--process-sentinel (proc _event)
  (unless (process-live-p proc)
    (let* ((buffer (process-buffer proc))
           (win (get-buffer-window buffer)))
      (if win
          (quit-window t win)
        (kill-buffer buffer)))))

(defun helm-lxc--process-file (&rest args)
  "Run `process-file' with `non-essential' set to nil.
Helm set `non-essential' to non-nil which prevent TRAMP from
opening connections."
  (let ((non-essential))
    (apply #'process-file args)))

(defun helm-lxc--process-lines (host program &optional delete-trailing-ws &rest args)
  (with-temp-buffer
    (let ((default-directory (or host default-directory)))
      (when (zerop (apply 'helm-lxc--process-file program nil t nil args))
        (when delete-trailing-ws
          (let ((delete-trailing-lines t))
            (delete-trailing-whitespace)))
        (goto-char (point-min))
        (let (lines)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (push line lines))
            (forward-line))
          (nreverse lines))))))

(defun helm-lxc--list-containers (host)
  (if-let ((containers (gethash host helm-lxc--cache)))
      containers
    (puthash host
             (let ((containers (helm-lxc--process-lines host "lxc-ls" t "-1")))
               (mapcar (lambda (c)
                         (let* ((info (helm-lxc--get-container-info host c))
                                (state (alist-get 'state info))
                                (face (helm-lxc--face-from-state state)))
                           (propertize c 'face face 'helm-lxc info)))
                       containers))
             helm-lxc--cache)))

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
                    (or (car (helm-lxc--process-lines
                              host
                              "lxc-info"
                              nil
                              "-i" "-H" "-n" container))
                        "")
                    ","
                    t))))
    `((host . ,host)
      (name . ,container)
      (state . ,state)
      (pid . ,pid)
      (ips . ,ips))))

(defun helm-lxc--get-candidates (&optional marked)
  (or (and marked (helm-marked-candidates :all-sources t))
      (list (helm-get-selection nil 'withprop))))

(defun helm-lxc--get-container-from-candidate (candidate)
  (get-text-property 0 'helm-lxc candidate))

(defun helm-lxc--clear-cache-for-candidate (candidate)
  (let* ((container (helm-lxc--get-container-from-candidate candidate))
         (host (alist-get 'host container)))
    (puthash host nil helm-lxc--cache)))

(defun helm-lxc--clear-cache (&optional _candidate)
  (let ((candidates (helm-lxc--get-candidates t)))
    (dolist (candidate candidates)
      (helm-lxc--clear-cache-for-candidate candidate))))

(cl-defun helm-lxc--create-action (action
                                   &optional (marked t) clear-cache
                                   &rest args)
  (lambda (candidate &optional from-chain)
    (let (rc)
      (dolist (candidate
               (or (and from-chain (list candidate))
                   (helm-lxc--get-candidates marked))
               rc)
        (let* ((container (helm-lxc--get-container-from-candidate candidate))
               (name (alist-get 'name container))
               (host (alist-get 'host container))
               (default-directory (or host default-directory))
               (cmd (format "lxc-%s" action)))
          (setq rc (apply #'helm-lxc--process-file
                          cmd nil nil nil "-n" name args))
          (unless (zerop rc)
            (message "helm-lxc: lxc-%s on %s failed with return code %d"
                     action name rc))
          (when clear-cache
            (helm-lxc--clear-cache-for-candidate candidate)))))))

(defun helm-lxc--create-action-chain (marked clear-cache &rest actions)
  (lambda (_candidate)
    (dolist (candidate (helm-lxc--get-candidates marked))
      (catch 'break
        (dolist (action actions)
          (let ((rc (pcase action
                      ((pred stringp)
                       (funcall (helm-lxc--create-action action nil clear-cache)
                                candidate t))
                      ((pred functionp)
                       (funcall action candidate))
                      (`(,name . ,args)
                       (funcall (apply #'helm-lxc--create-action
                                       name nil clear-cache args)
                                candidate t))
                      (bad (error "Bad action: %S" bad)))))
            (unless (and rc (zerop rc))
              (throw 'break rc))))))))

(defun helm-lxc--attach (_candidate)
  (let* ((candidate (car (helm-lxc--get-candidates)))
         (container (helm-lxc--get-container-from-candidate candidate))
         (name (alist-get 'name container))
         (host (alist-get 'host container))
         (ip (car (alist-get 'ips container)))
         (use-ssh (and helm-lxc-attach-with-ssh ip))
         (attach-method (or (and use-ssh "ssh") "lxc"))
         (attach-user (or (and use-ssh helm-lxc-attach-ssh-user) "root"))
         (attach-host (or (and use-ssh ip) name))
         (default-directory (concat
                             (and host (format "%s|" (substring host 0 -1)))
                             (format "%s:%s@%s:"
                                     attach-method
                                     attach-user
                                     attach-host)))
         (shell-file-name "/bin/bash")
         (buffer (shell (format "*shell %s@%s*" attach-user name)))
         (proc (get-buffer-process buffer)))
    (when (and proc helm-lxc-clean-up-on-shell-exit)
      (add-function :after (process-sentinel proc) #'helm-lxc--process-sentinel))))

(defun helm-lxc--connect-to-host (_candidate)
  (let* ((candidate (car (helm-lxc--get-candidates)))
         (container (helm-lxc--get-container-from-candidate candidate))
         (host (alist-get 'host container))
         (user (or (file-remote-p host 'user) "unknown"))
         (hostname (file-remote-p host 'host))
         (default-directory (or host default-directory))
         (shell-file-name "/bin/bash")
         (buffer (shell (format "*shell %s@%s*" user hostname)))
         (proc (get-buffer-process buffer)))
    (when (and proc helm-lxc-clean-up-on-shell-exit)
      (add-function :after (process-sentinel proc) #'helm-lxc--process-sentinel))))

(defun helm-lxc--show-container-info (_candidate)
  (let* ((buffer (get-buffer-create "*helm lxc info*"))
         (candidate (car (helm-lxc--get-candidates)))
         (container (helm-lxc--get-container-from-candidate candidate)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Name: %s\n" (alist-get 'name container)))
      (insert (format "Host: %s\n" (file-remote-p (alist-get 'host container) 'host)))
      (insert (format "State: %s\n" (alist-get 'state container)))
      (when-let ((pid (alist-get 'pid container)))
        (insert (format "PID: %s\n" pid)))
      (when-let ((ips (alist-get 'ips container)))
        (insert (format "IPs: %s\n" (string-join ips " ")))))
    (pop-to-buffer buffer)))

(defvar helm-lxc--action-destroy
  (helm-lxc--create-action "destroy" t t))

(defvar helm-lxc--action-freeze
  (helm-lxc--create-action-chain
   t
   t
   "freeze"
   '("wait" "-s" "FROZEN")))

(defvar helm-lxc--action-unfreeze
  (helm-lxc--create-action-chain
   t
   t
   "unfreeze"
   '("wait" "-s" "RUNNING")))

(defvar helm-lxc--action-unfreeze-and-attach
  (helm-lxc--create-action-chain
   t
   t
   helm-lxc--action-unfreeze
   'helm-lxc--attach))

(defvar helm-lxc--action-stop
  (helm-lxc--create-action-chain
   t
   t
   "stop"
   '("wait" "-s" "STOPPED")))

(defvar helm-lxc--action-stop-and-destroy
  (helm-lxc--create-action-chain
   t
   t
   helm-lxc--action-stop
   helm-lxc--action-destroy))

(defvar helm-lxc--action-start
  (helm-lxc--create-action-chain
   t
   t
   '("start" "-d")
   '("wait" "-s" "RUNNING")))

(defvar helm-lxc--action-start-and-attach
  (helm-lxc--create-action-chain
   t
   t
   helm-lxc--action-start
   'helm-lxc--attach))

(defvar helm-lxc--action-restart
  (helm-lxc--create-action-chain
   t
   t
   helm-lxc--action-stop
   helm-lxc--action-start))

(defvar helm-lxc--action-restart-and-attach
  (helm-lxc--create-action-chain
   t
   t
   helm-lxc--action-stop
   helm-lxc--action-start
   'helm-lxc--attach))

(defun helm-lxc--action-transformer (_actions _candidate)
  (let* ((candidate (car (helm-lxc--get-candidates)))
         (container (helm-lxc--get-container-from-candidate candidate))
         (connect-action '("Connect to host" . helm-lxc--connect-to-host)))
    (pcase (alist-get 'state container)
      ("running" `(("Attach" . helm-lxc--attach)
                   ("Stop" . ,helm-lxc--action-stop)
                   ("Stop and destroy" . ,helm-lxc--action-stop-and-destroy)
                   ("Restart" . ,helm-lxc--action-restart)
                   ("Restart and attach" . ,helm-lxc--action-restart-and-attach)
                   ("Freeze" . ,helm-lxc--action-freeze)
                   ,connect-action))
      ("stopped" `(("Start and attach" . ,helm-lxc--action-start-and-attach)
                   ("Start" . ,helm-lxc--action-start)
                   ("Destroy" . ,helm-lxc--action-destroy)
                   ,connect-action))
      ("frozen" `(("Unfreeze and attach" . ,helm-lxc--action-unfreeze-and-attach)
                  ("Unfreeze" . ,helm-lxc--action-unfreeze)
                  ,connect-action)))))

(defun helm-lxc--build-sources ()
  (cl-loop for (name . tramp-path) in helm-lxc-hosts
           collect
           (helm-build-in-buffer-source name
             :init (let ((tramp-path-1 tramp-path))
                     (lambda ()
                       (helm-init-candidates-in-buffer
                           'global
                         (helm-lxc--list-containers tramp-path-1))))
             :keymap helm-lxc-map
             :get-line 'buffer-substring
             :marked-with-props 'withprop
             :action-transformer 'helm-lxc--action-transformer
             :persistent-action 'helm-lxc--show-container-info
             :persistent-help "Show container info")))

(defun helm-lxc-start-persistent ()
  "Start container without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'start-action helm-lxc--action-start)
    (helm-execute-persistent-action 'start-action)
    (helm-lxc--clear-cache)
    (helm-force-update)))
(put 'helm-lxc-start-persistent 'helm-only t)

(defun helm-lxc-stop-persistent ()
  "Stop a container without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'stop-action helm-lxc--action-stop)
    (helm-execute-persistent-action 'stop-action)
    (helm-lxc--clear-cache)
    (helm-force-update)))
(put 'helm-lxc-stop-persistent 'helm-only t)

(defun helm-lxc-clear-cache-persistent ()
  "Clear the cache without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'clear-cache-action #'helm-lxc--clear-cache)
    (helm-execute-persistent-action 'clear-cache-action)
    (helm-force-update)))
(put 'helm-lxc-clear-cache-persistent 'helm-only t)

;;;###autoload
(defun helm-lxc ()
  (interactive)
  (helm
   :buffer "*helm lxc*"
   :sources (helm-lxc--build-sources)))

(provide 'helm-lxc)

;;; helm-lxc.el ends here
