;;; setup-elfeed -- Summary
;;; Commentary:
;;; Elfeed.
;;; Code:
(require 'use-package)

(use-package elfeed-org
  :defer t
  :config (progn
            (elfeed-org)
            (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))))

(provide 'setup-elfeed)
;;; setup-elfeed.el ends here
