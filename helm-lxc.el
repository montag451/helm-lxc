;;; helm-lxc.el --- Helm interface to manage LXC containers -*- lexical-binding: t -*-

;; Copyright (C) 2018 montag451

;; Author: montag451
;; URL: https://github.com/montag451/helm-lxc
;; Keywords: helm, lxc, convenience
;; Version: 0.1.0
;; Package-Requires: (helm lxc-tramp cl-lib s)

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
(require 's)
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
If non-nil, when a shell exits its buffer is killed and its
window, if any, is deleted."
  :group 'helm-lxc
  :type 'boolean)

(defcustom helm-lxc-attach-with-ssh nil
  "Attach to the container using SSH instead of lxc-tramp.
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
    ("running" 'helm-lxc-face-running)
    ("stopped" 'helm-lxc-face-stopped)
    ("frozen" 'helm-lxc-face-frozen)))

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
  (let ((containers (helm-lxc--process-lines host "lxc-ls" t)))
    (mapcar (lambda (c)
              (let* ((info (helm-lxc--get-container-info host c))
                     (state (alist-get 'state info))
                     (face (helm-lxc--face-from-state state)))
                (cons (propertize c 'face face) info)))
            containers)))

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
         (buffer (shell (format "*shell %s@%s*" name source)))
         (proc (get-buffer-process buffer)))
    (when helm-lxc-clean-up-on-shell-exit
      (add-function :after (process-sentinel proc) #'helm-lxc--process-sentinel))))

(defun helm-lxc--execute-action (action &rest args)
  (lambda (_)
    (dolist (container (helm-marked-candidates :all-sources t))
      (let* ((name (alist-get 'name container))
             (host (alist-get 'host container))
             (default-directory (or host default-directory)))
        (unless (zerop (apply #'process-file (format "lxc-%s" action) nil nil nil "-n" name args))
          (message "lxc-%s on %s failed" action name))))))

(defmacro helm-lxc--chain-actions (&rest actions)
  `(lambda (c)
     (dolist (action ',actions)
       (pcase action
         ((pred stringp) (funcall (helm-lxc--execute-action action) c))
         ((pred functionp) (funcall action c))
         (`(,name . ,args) (funcall (apply #'helm-lxc--execute-action name args) c))
         (bad (error "Bad action: %S" bad))))))

(defun helm-lxc--connect-to-host (container)
  (let* ((host (alist-get 'host container))
         (default-directory (or host default-directory))
         (shell-file-name "/bin/bash")
         (buffer (shell (format "*shell %s*" host)))
         (proc (get-buffer-process buffer)))
    (when helm-lxc-clean-up-on-shell-exit
      (add-function :after (process-sentinel proc) #'helm-lxc--process-sentinel))))

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
      ("running" `(("Attach" . helm-lxc--attach)
                   ("Stop" . ,(helm-lxc--execute-action "stop"))
                   ("Stop and destroy" . ,(helm-lxc--chain-actions
                                           "stop"
                                           ("wait" "-s" "STOPPED")
                                           "destroy"))
                   ("Restart" . ,(helm-lxc--chain-actions
                                  "stop"
                                  ("wait" "-s" "STOPPED")
                                  ("start" "-d")))
                   ("Restart and attach" . ,(helm-lxc--chain-actions
                                             "stop"
                                             ("wait" "-s" "STOPPED")
                                             ("start" "-d")
                                             ("wait" "-s" "RUNNING")
                                             helm-lxc--attach))
                   ("Freeze" . ,(helm-lxc--execute-action "freeze"))
                   ,connect-action))
      ("stopped" `(("Start and attach" . ,(helm-lxc--chain-actions
                                           ("start" "-d")
                                           ("wait" "-s" "RUNNING")
                                           helm-lxc--attach))
                   ("Start" . ,(helm-lxc--execute-action "start" "-d"))
                   ("Destroy" . ,(helm-lxc--execute-action "destroy"))
                   ,connect-action))
      ("frozen" `(("Unfreeze and attach" . ,(helm-lxc--chain-actions
                                             "unfreeze"
                                             ("wait" "-s" "RUNNING")
                                             helm-lxc--attach))
                  ("Unfreeze" . ,(helm-lxc--execute-action "unfreeze"))
                  ,connect-action)))))

(defun helm-lxc--build-sources ()
  (cl-loop for (name . tramp-path) in helm-lxc-hosts
           collect
           (helm-build-sync-source name
             :candidates (helm-lxc--list-containers tramp-path)
             :action-transformer 'helm-lxc--action-transformer
             :persistent-action 'helm-lxc--show-container-info
             :persistent-help "Show container info")))

;;;###autoload
(defun helm-lxc ()
  (interactive)
  (helm
   :buffer "*helm lxc*"
   :sources (helm-lxc--build-sources)))

;;; helm-lxc.el ends here
