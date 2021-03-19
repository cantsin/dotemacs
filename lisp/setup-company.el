;;; setup-company -- Summary
;;; Commentary:
;;; Setup auto completion.
;;; Code:
(require 'use-package)

(defun next-company-completion ()
  "Select the next completion item."
  (interactive)
  (company-complete-common-or-cycle 1))

(defun prev-company-completion ()
  "Select the previous completion item."
  (interactive)
  (company-complete-common-or-cycle -1))

(use-package company
  :demand t
  :hook (after-init . global-company-mode)
  :bind (("C-/" . company-complete)
         :map company-active-map ("C-n" . next-company-completion)
         :map company-active-map ("C-p" . prev-company-completion))
  :config
  (setq company-idle-delay 0
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-show-numbers t
        company-selection-wrap-around t
        company-backends (list #'company-files
                               (list #'company-dabbrev-code
                                     #'company-gtags
                                     #'company-etags
                                     #'company-keywords)
                               #'company-dabbrev)))

(use-package company-nixos-options
  :config
  (add-to-list 'company-backends 'company-nixos-options))

;; custom company backend for ledger mode
(require 'company-ledger-accounts)

(provide 'setup-company)
;;; setup-company.el ends here
