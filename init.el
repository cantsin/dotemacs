;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; global settings.
(global-auto-revert-mode 1)
(global-font-lock-mode t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
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
(setq tooltip-use-echo-area t)
(setq color-theme-is-global t)
(setq sentence-end-double-space t)
(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)
(setq mouse-yank-at-point t)

(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(delete-selection-mode)
(auto-compression-mode t)
(tooltip-mode -1)
(hl-line-mode t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

;; set up our own site-lisp.
(let ((default-directory (concat user-emacs-directory "site-lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; add marmalade to package-archives.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(confluence
    diminish
    dired+
    expand-region
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    haskell-mode
    hl-line+
    ido-ubiquitous
    offlineimap
    openwith
    paredit
    session
    smart-tab
    smex
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

;; install manually:
;; - bbdb
;; - julia-mode.el
;; - magit
;; - notmuch
;; - org-bullets.el
;; - session
;; - stripe-buffer.el

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;; places.
(setq save-place-file (concat user-emacs-directory "places"))

;; save our customizations elsewhere.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; load our other setup files.
(add-to-list 'load-path user-emacs-directory)
(require 'setup-private)
(require 'setup-magit)
(require 'setup-windmove)
(require 'setup-misc)
(require 'setup-smex)
(require 'setup-dired)
(require 'setup-ido)
(require 'setup-erc)
;;(require 'setup-gnus)
(require 'setup-org)
(require 'setup-languages)

;; diminish mode ftw
(require 'diminish)
(diminish 'eldoc-mode)
;(diminish 'paredit-mode "()")
;(diminish 'smart-tab-mode)
