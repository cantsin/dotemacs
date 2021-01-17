;;; setup-mail -- Summary
;;; Commentary:
;;; Setup email configuration.
;;; Code:
(require 'use-package)

(use-package notmuch
  :config
  (setq notmuch-hello-thousands-separator ","
        notmuch-hello-sections
             (list #'notmuch-hello-insert-saved-searches
	           #'notmuch-hello-insert-search
	           #'notmuch-hello-insert-recent-searches
	           #'notmuch-hello-insert-alltags)))

(provide 'setup-mail)
;;; setup-mail.el ends here
