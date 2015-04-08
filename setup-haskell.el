;;; setup-haskell -- Summary
;;; Commentary:
;;; Setup for Haskell and related.
;;; Code:
(require 'use-package)

(use-package purscheck
  :defer t
  :init (add-to-list 'load-path "purscheck.el")) ;; custom

(defun cantsin/setup-purescript ()
  "Set up purescript."
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  (load "purescript-mode-autoloads"))

(use-package purescript-mode
  :defer t
  :ensure t
  :config (cantsin/setup-purescript))

(defun cantsin/setup-haskell ()
  "Set up haskell."
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package haskell-mode
  :defer t
  :init (cantsin/setup-haskell))

(provide 'setup-haskell)
;;; setup-haskell.el ends here
