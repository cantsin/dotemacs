;;; init -- summary
;;; Commentary:
;;; set generic defaults then default to setup-* modules

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; use C-mouse-3 to pop up entire menu-bar as popup menu

;; global settings.
(global-auto-revert-mode 1)
(global-font-lock-mode t)
(global-subword-mode 1)

;; various settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      use-file-dialog nil
      use-dialog-box nil
      visible-bell '1
      truncate-lines t
      truncate-partial-width-windows t
      make-backup-files nil
      font-lock-maximum-decoration t
      line-number-mode t
      column-number-mode t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      show-trailing-whitespace t
      echo-keystrokes 0.1
      shift-select-mode nil
      sentence-end-double-space t
      mouse-yank-at-point t
      x-select-enable-clipboard t
      fill-column 80
      vc-make-backup-files t
      uniquify-buffer-name-style 'forward)

(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(delete-selection-mode)
(auto-compression-mode t)
(tooltip-mode nil)
(hl-line-mode t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'sentence-end-double-space nil)

;; enable some disabled commands.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; UTF-8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(add-hook 'text-mode-hook 'prettify-symbols-mode)

;; global hooks.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
(add-hook 'makefile-mode-hook
          'indent-tabs-mode)
(add-hook 'text-mode-hook
          'turn-on-auto-fill)
(add-hook 'focus-out-hook '(lambda ()
                             (interactive)
                             (save-some-buffers t)))

;; write backup files to its own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; set window title!
(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))

;; cask/pallet to manage our installed packages.
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; load our other setup files.
(add-to-list 'load-path (concat user-emacs-directory "."))
(require 'setup-theme)
(require 'setup-buffer)
(require 'setup-fly)
(require 'setup-magit)
(require 'setup-edit)
(require 'setup-dired)
(require 'setup-helm)
(require 'setup-erc)
(require 'setup-eshell)
(require 'setup-org)
(require 'setup-twitter)
(require 'setup-company)
(require 'setup-languages)
(require 'setup-go)
(require 'setup-lisp)
(require 'setup-haskell)
(require 'setup-projectile)
(require 'setup-session)
(require 'setup-keys)
(require 'setup-functions)

;; load only if available
(if (file-exists-p "~/.emacs.d/setup-private.el")
    (require 'setup-private))
(when (require 'mu4e nil 'noerror)
  (require 'setup-mu4e))

;; save our customizations elsewhere.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
