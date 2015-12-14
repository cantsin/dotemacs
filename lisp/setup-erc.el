;;; setup-erc -- Summary
;;; Commentary:
;;; Setup all things ERC-related.
;;; Code:
(require 'use-package)

;;; Notify me when a keyword is matched (someone wants to reach me)
(defvar my-erc-page-message "%s is calling your name."
  "Format of message to display in dialog box.")

(defvar my-erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a notification.")

(defvar my-erc-page-timeout 30
  "Number of seconds that must elapse between notifications from the same person.")

(defun my-erc-page-popup-notification (nick)
  "Notify using the WM.  NICK should be part of the message."
  (when window-system
    ;; must set default directory, otherwise start-process is unhappy
    ;; when this is something remote or nonexistent
    (let ((default-directory "~/"))
      ;; 8640000 milliseconds = 1 day
      (start-process "page-me" nil "notify-send"
                     "-u" "normal" "-t" "8640000" "ERC"
                     (format my-erc-page-message nick)))))

(defun my-erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-erc-page-timeout'."
  (unless delay (setq delay my-erc-page-timeout))
  (let ((cur-time (float-time (current-time)))
        (cur-assoc (assoc nick my-erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) my-erc-page-nick-alist)
      t)))

(defun my-erc-page-me (match-type nick message)
  "Notify the user when someone sends a message that \\
will match a regexp in `erc-keywords'.  Parameters: MATCH-TYPE, \\
NICK, MESSAGE."
  (interactive)
  (when (and (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (null (string-match "\\`\\([sS]erver\\|localhost\\)" nick))
             ;; or bots
             (null (string-match "\\(bot\\|serv\\)!" nick))
             ;; or from those who abuse the system
             (my-erc-page-allowed nick))
    (my-erc-page-popup-notification nick)))

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  "Save the ERC logs."
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))

(defun my-erc-page-me-PRIVMSG (proc parsed)
  "Page the user if a PRIVMSG arrives.  PROC is not used.  PARSED is the message."
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (my-erc-page-allowed nick))
      (my-erc-page-popup-notification nick)
      nil)))
(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)

;; custom command.
(defun irc ()
  "Connect to IRC."
  (interactive)
  (znc-all))

(defun cantsin/erc-setup ()
  "Set up ERC."
  (add-hook 'erc-text-matched-hook 'my-erc-page-me)
  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

  (erc-spelling-mode 1)
  (erc-autojoin-mode 1)
  (erc-match-mode 1)
  (erc-fill-mode t)
  (erc-ring-mode t)
  (erc-netsplit-mode t)
  (erc-timestamp-mode t))

(defun cantsin/erc-config ()
  "Configure ERC."
  (setq  erc-timestamp-format "[%D %r]"
         ;; logging
         erc-log-insert-log-on-open nil
         erc-log-channels-directory "~/.irc_logs/"
         erc-save-buffer-on-part t
         erc-hide-timestamps nil
         erc-auto-query 'buffer
         ;; annoying when you have large channels
         erc-hide-list '("JOIN" "PART" "QUIT")))

(use-package erc
  :defer t
  :init (cantsin/erc-setup)
  :config (cantsin/erc-config))

(provide 'setup-erc)
;;; setup-erc.el ends here