;;; setup-languages -- Summary
;;; Commentary:
;;; Setup for various languages.
;;; Code:
(require 'use-package)

(use-package lsp-mode
  :bind (("M-." . lsp-find-definition))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config (progn
            (require 'lsp-ui-sideline)
            (set-face-background 'lsp-ui-sideline-global "gray14"))
  :init (progn
           (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

(use-package lua-mode
  :defer t
  :init (setq lua-indent-level 2))

(use-package alchemist
  :defer t
  :init (setq alchemist-project-compile-when-needed t))

(defun cantsin/setup-jedi ()
  "Configure jedi (python)."
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package jedi
  :defer t
  :config (cantsin/setup-jedi))

(use-package compile
  :defer t
  :init (setq compilation-scroll-output t))

(use-package restclient
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode)))

;; auto disassemble llvm when opening .bc files
(use-package autodisass-llvm-bitcode
  :defer t)

(defun load-agda ()
  "Load agda-mode on demand."
  (interactive)
  (condition-case nil
      (load-file (let ((coding-system-for-read 'utf-8))
                   (shell-command-to-string "agda-mode locate")))
    (error nil)))

(defun my-makefile-hook ()
  "Use tabs."
  (setq indent-tabs-mode t))
(add-hook 'makefile-mode-hook 'my-makefile-hook)

(use-package cc-mode
  :config (progn
            (setq c-basic-offset 4)
            (c-set-offset 'substatement-open 0)))

(use-package fsharp-mode
  :defer t
  :config (progn
            (setq fsharp-indent-offset 2)))

(use-package rust-mode
  :defer t
  :init
  (progn (add-hook 'rust-mode-hook #'cargo-minor-mode)
         (add-hook 'rust-mode-hook #'lsp)
         (setq rust-format-on-save t)))

(use-package nix-update
  :defer t)

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(provide 'setup-languages)
;;; setup-languages.el ends here
