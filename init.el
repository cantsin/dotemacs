;;; init -- summary
;;; Commentary:
;;; set generic defaults then default to setup-* modules

;;; Code:

(require 'package)
(unless package--initialized (package-initialize t))

(setq gc-cons-threshold 100000000)
(setq garbage-collection-messages t)
(setq read-process-output-max 1048576)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; use C-mouse-3 to pop up entire menu-bar as popup menu

;; global settings.
(global-auto-revert-mode 1)
(global-font-lock-mode t)
(global-subword-mode 1)

;; various settings.
(setq column-number-mode t
      echo-keystrokes 0.1
      fill-column 80
      font-lock-maximum-decoration t
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-compacting-font-caches t
      initial-scratch-message nil
      line-number-mode t
      make-backup-files nil
      mouse-yank-at-point t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      sentence-end-double-space t
      shift-select-mode nil
      show-trailing-whitespace t
      truncate-lines t
      truncate-partial-width-windows t
      uniquify-buffer-name-style 'forward
      use-dialog-box nil
      use-file-dialog nil
      vc-make-backup-files t
      visible-bell '1
      select-enable-clipboard t)

(auto-compression-mode t)
(blink-cursor-mode 0)
(delete-selection-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(hl-line-mode t)
(tooltip-mode nil)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'sentence-end-double-space nil)
(set-default 'left-margin-width 1)
(set-default 'right-margin-width 1)
(set-default 'fringes-outside-margins t)
(set-window-buffer nil (current-buffer))

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

;; global hooks.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
(add-hook 'focus-out-hook
          '(lambda ()
             (interactive)
             (save-some-buffers t)))

;; The default "C-x c" is too close to "C-x C-c", which quits Emacs.
(global-unset-key (kbd "C-x c"))

;; disable overwrite-mode because it is really annoying.
(define-key global-map [(insert)] nil)

;; remap backspace key to something more sensible
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; make {back,for}ward-paragraph easier to use
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(global-set-key (kbd "<mouse-9>") 'next-buffer)
(global-set-key (kbd "<mouse-8>") 'previous-buffer)

;; write backup files to its own directory
(setq auto-save-default nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setenv "PATH" (concat (getenv "PATH") ":/home/james/bin"))
(setq exec-path (append exec-path '("/home/james/bin")))

;; load our other setup files.
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/custom"))
(require 'setup-fly)
(require 'setup-projectile)
(require 'setup-theme)

;; font setup
(defun get-font-size ()
  "Get font size for this system."
  (if (member (system-name) '("satori")) 12 16))
(defun get-font (&optional size)
  "Get font for this system, can be overridden with SIZE."
  (format "Triplicate T3c-%d" (or size (get-font-size))))
(let*
    ((font-size (get-font-size))
     (current-font (get-font font-size))
     (current-sub-font (get-font (- font-size 2))))
  (add-to-list 'default-frame-alist `(font . ,current-font))
  (set-face-attribute 'default t :font current-font)
  (set-face-attribute 'default nil :font current-font)
  (set-frame-font current-font nil t)
  (set-face-attribute 'mode-line nil :font current-sub-font)
  (set-face-attribute 'mode-line-inactive nil :font current-sub-font))

(require 'setup-buffer)
(require 'setup-magit)
(require 'setup-dired)
(require 'setup-ivy)
(require 'setup-org)
(require 'setup-company)
(require 'setup-languages)
(require 'setup-frontend)
(require 'setup-lisp)
(require 'setup-haskell)
(require 'setup-keys)
(require 'setup-functions)
(require 'setup-mail)

(if (file-exists-p (concat user-emacs-directory "lisp/setup-private.el"))
    (require 'setup-private))

;; save our customizations elsewhere.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
