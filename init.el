;;; init -- summary
;;; Commentary:
;;; set generic defaults then default to setup-* modules

;;; Code:

;; as of December 12 2015:
;; https://github.com/syl20bnr/spacemacs/issues/3854
(package-initialize)

;; cask/pallet to manage our installed packages.
(let ((default-directory (concat user-emacs-directory ".cask/")))
  (normal-top-level-add-subdirs-to-load-path))
(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(setq gc-cons-threshold 100000000)

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
      x-select-enable-clipboard t)

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
(add-hook 'focus-out-hook
          '(lambda ()
             (interactive)
             (save-some-buffers t)))

;; write backup files to its own directory
(setq auto-save-default nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

(setenv "PATH" (concat (getenv "PATH") ":/home/james/bin"))
(setq exec-path (append exec-path '("/home/james/bin")))

;; load our other setup files.
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(require 'setup-theme)
(require 'setup-buffer)
(require 'setup-fly)
(require 'setup-magit)
(require 'setup-dired)
(require 'setup-helm)
(require 'setup-eshell)
(require 'setup-org)
(require 'setup-company)
(require 'setup-languages)
(require 'setup-lisp)
(require 'setup-haskell)
(require 'setup-projectile)
(require 'setup-edit)
(require 'setup-session)
(require 'setup-keys)
(require 'setup-functions)

;; load only if available
(if (file-exists-p (concat user-emacs-directory "lisp/setup-private.el"))
    (require 'setup-private))

(defun my-idris-mode-hook ()
  ;; This makes it so that especially errors reuse their frames
  ;; https://emacs.stackexchange.com/questions/327/how-can-i-block-a-frame-from-being-split/338
  ;; alternatively, add this to certain frames: (set-frame-parameter nil 'unsplittable t)
  ;; (without this, idris throws out tons of new frames)
  (add-to-list 'display-buffer-alist
               '(".*". (display-buffer-reuse-window . ((reusable-frames . t)))))
  (setq idris-stay-in-current-window-on-compiler-error t)
  (setq idris-prover-restore-window-configuration t)

  ;; If you kill a buffer (eg, hit "q"), frames with these names wil also be killed
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris-repl*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris-notes*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris-info*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris-holes*"))

(add-hook 'idris-mode-hook #'my-idris-mode-hook)

;; save our customizations elsewhere.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
