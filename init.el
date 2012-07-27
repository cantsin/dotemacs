;; global settings.
(global-auto-revert-mode 1)
(global-font-lock-mode t)

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq visible-bell '1)
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
(setq make-backup-files nil)
(setq font-lock-maximum-decoration t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq show-trailing-whitespace t)
(setq echo-keystrokes 0.1)
(setq shift-select-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(delete-selection-mode)
(set-scroll-bar-mode nil)
(auto-compression-mode t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

;; set up our own site-lisp.
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; add marmalade to package-archives.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; global hooks.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; because there's no other way to run emacs.
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(toggle-fullscreen)

;; theme.
(load-theme 'zenburn t)

;; save our customizations elsewhere.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; load our other setup files.
(add-to-list 'load-path "~/.emacs.d/")
(require 'setup-magit)
(require 'setup-windmove)
(require 'setup-misc)
(require 'setup-smex)
(require 'setup-ido)
