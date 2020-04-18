;;; setup-frontend -- Summary
;;; Commentary:
;;; Setup for the front end
;;; Code:
(require 'use-package)

(use-package js2-mode
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :init (progn
          (add-hook 'js-mode-hook 'js2-minor-mode)))

(use-package rjsx-mode
  :defer t
  :mode ("\\.jsx\\'" . rjsx-mode)
  :init (progn
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
  :mode ("\\.html\\'" . web-mode)
  :init
  (setq css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing nil
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-engine "django"
        web-mode-indent-style 2))

(provide 'setup-frontend)
;;; setup-frontend.el ends here
