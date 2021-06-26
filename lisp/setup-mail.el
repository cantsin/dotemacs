;;; setup-mail -- Summary
;;; Commentary:
;;; Setup email configuration.
;;; Code:
(require 'use-package)

(use-package notmuch
  :config
  (add-hook 'notmuch-message-mode-hook 'company-mode)
  (setq notmuch-hello-thousands-separator ","
        notmuch-address-command nil
        notmuch-address-use-company nil ;; do not use notmuch-company for completion
        notmuch-search-oldest-first nil
        notmuch-fcc-dirs
        '(("jtranovich@gmail.com" . "\"gmail/[Gmail]/Sent Mail\" -inbox +sent -unread")
          ("james@tranovich.com" . "\"fastmail/Sent Items\" -inbox +sent -unread"))
        notmuch-hello-sections
             (list #'notmuch-hello-insert-saved-searches
	           #'notmuch-hello-insert-search
	           #'notmuch-hello-insert-recent-searches
	           #'notmuch-hello-insert-alltags)))

(setq message-kill-buffer-on-exit t
      send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      message-sendmail-envelope-from 'header
      user-full-name "James Tranovich"
      mail-specify-envelope-from t
      mail-host-address "tranovich.com"
      mail-envelope-from 'header)

;; custom company backend for contacts
(require 'khard-contacts)

(provide 'setup-mail)
;;; setup-mail.el ends here
