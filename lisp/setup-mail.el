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
        notmuch-hello-sections
             (list #'notmuch-hello-insert-saved-searches
	           #'notmuch-hello-insert-search
	           #'notmuch-hello-insert-recent-searches
	           #'notmuch-hello-insert-alltags)))

(setq message-kill-buffer-on-exit t)

;; custom company backend for contacts
(require 'khard-contacts)

(provide 'setup-mail)
;;; setup-mail.el ends here
