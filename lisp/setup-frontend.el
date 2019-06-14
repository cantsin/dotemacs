;;; setup-frontend -- Summary
;;; Commentary:
;;; Setup for the front end
;;; Code:
(require 'use-package)

(use-package js2-mode
  :defer t
  :init (progn
          (add-hook 'js-mode-hook 'js2-minor-mode)))

(use-package rjsx-mode
  :defer t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
          (setq-default js2-basic-offset 2)
          (setq js-indent-level 2)))

(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :defer t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package web-mode
  :defer t
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-indent-style 2)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  :config
  (smartparens-mode 0))

(provide 'setup-frontend)
;;; setup-frontend.el ends here
