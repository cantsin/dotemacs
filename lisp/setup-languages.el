;;; setup-languages -- Summary
;;; Commentary:
;;; Setup for various languages.
;;; Code:
(require 'use-package)

(add-hook 'makefile-mode-hook '(lambda () (setq indent-tabs-mode t)))

(defun load-agda ()
  "Load agda-mode on demand."
  (interactive)
  (condition-case nil
      (load-file (let ((coding-system-for-read 'utf-8))
                   (shell-command-to-string "agda-mode locate")))
    (error nil)))

(use-package alchemist
  :defer t)

;; auto disassemble llvm when opening .bc files
(use-package autodisass-llvm-bitcode
  :defer t)

(use-package cargo-mode
  :defer t)

(use-package cc-mode
  :config
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(use-package compile
  :defer t
  :init (setq compilation-scroll-output t))

(use-package fsharp-mode
  :defer t
  :config
  (setq fsharp-indent-level 2))

(use-package jedi
  :defer t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package lsp-mode
  :demand t
  :hook (rust-mode . lsp)
  :bind (("M-." . lsp-find-definition))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (require 'lsp-ui-sideline)
  (set-face-background 'lsp-ui-sideline-global "gray14")
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lua-mode
  :defer t
  :init
  (setq lua-indent-level 2))

(use-package nix-mode
  :defer t
  :mode ("\\.nix\\'" . nix-mode))

(use-package nix-update
  :defer t)

(use-package racer
  :defer t
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package restclient
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode)))

(use-package rust-mode
  :defer t
  :bind (("M-\"" . racer-find-definition))
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-format-on-save t)
  :config
  (add-hook 'rust-mode-hook #'racer-mode))

(provide 'setup-languages)
;;; setup-languages.el ends here
