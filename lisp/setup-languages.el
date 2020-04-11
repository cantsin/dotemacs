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

(use-package cargo
  :defer t)

(use-package cc-mode
  :bind
  (:map c-mode-map ("M-." . xref-find-definitions)
   :map c-mode-map ("M-?" . xref-find-references))
  :config
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(use-package compile
  :defer t
  :init (setq compilation-scroll-output t))

(use-package dap-mode
  :defer t)

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
  :bind (("M-." . lsp-find-definition)
         ("M-?" . lsp-find-references))
  :config
  (setq lsp-enable-snippet nil)
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

(use-package kotlin-mode
  :defer t
  :mode ("\\.kt\\'" . kotlin-mode)
  :init
  (company-mode +1)
  (add-hook 'kotlin-mode-hook #'lsp))

(use-package merlin
  :defer t
  :mode ("\\.ml\\'" "\\.mli\\'")
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'caml-mode-hook 'merlin-mode))

(defun nix-format ()
  "Nix format the buffer."
  (interactive)
  (when (string= mode-name "Nix")
    (shell-command (format "nixfmt %s" (buffer-file-name)))
    (revert-buffer :ignore-auto :noconfirm)))

(use-package nix-mode
  :defer t
  :mode ("\\.nix\\'" . nix-mode)
  :config
  (add-hook 'after-save-hook #'nix-format))

(use-package nix-update
  :defer t)

(use-package restclient
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode)))

(use-package rustic)

(defun zig-format ()
  "Zig format the buffer."
  (interactive)
  (when (string= mode-name "Zig")
    (shell-command (format "zig fmt %s" (buffer-file-name)))
    (revert-buffer :ignore-auto :noconfirm)))

(use-package zig-mode
  :defer t
  :mode ("\\.zig\\'" . zig-mode)
  :config
  (add-hook 'after-save-hook #'zig-format))

(provide 'setup-languages)
;;; setup-languages.el ends here
