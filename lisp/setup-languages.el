;;; setup-languages -- Summary
;;; Commentary:
;;; Setup for various languages.
;;; Code:
(require 'use-package)

(setq gdb-many-windows t)
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
  :bind (("M-." . lsp-find-definition)
         ("M-?" . lsp-find-references))
  :config
  (setq lsp-enable-snippet nil)
  :commands lsp)

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

(defun in-nix-shell-p ()
  "Are we in a nix shell?"
  (string-equal (getenv "IN_NIX_SHELL") "impure"))

(setq merlin-site-elisp (getenv "MERLIN_SITE_LISP"))

(use-package merlin
  :defer t
  :if (and merlin-site-elisp (in-nix-shell-p))
  :mode ("\\.ml\\'" "\\.mli\\'")
  :load-path merlin-site-elisp
  :hook
  (tuareg-mode . merlin-mode)
  (merlin-mode . company-mode)
  :custom
  (merlin-command "ocamlmerlin"))

(setq utop-site-elisp (getenv "UTOP_SITE_LISP"))

(use-package utop
  :defer t
  :if (and utop-site-elisp (in-nix-shell-p))
  :mode ("\\.ml\\'" "\\.mli\\'")
  :load-path utop-site-elisp
  :hook
  (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam config exec -- dune utop . -- -emacs"))

(setq ocp-site-elisp (getenv "OCP_INDENT_SITE_LISP"))

(use-package ocp-indent
  :defer t
  :if (and ocp-site-elisp (in-nix-shell-p))
  :mode ("\\.ml\\'" "\\.mli\\'")
  :load-path ocp-site-elisp)

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

;; work around a bug where direnv is called too late
(add-hook 'rustic-mode-hook
          (lambda () (progn (direnv-update-environment) (lsp))))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-format-trigger 'on-save))

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
