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

;; remap backspace key to something more sensible
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; easy access!
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; this should be already part of emacs.
(defun copy-line (&optional arg)
  "Copy the line, passing on ARG."
  (interactive "P")
  (save-excursion
    (read-only-mode t)
    (kill-line arg)
    (read-only-mode nil)))
(setq-default kill-read-only-ok t) ;; required for copy-line.
(global-set-key "\C-c\C-k" 'copy-line)

;; improve control-a
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

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
      vc-make-backup-files t)

(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(delete-selection-mode)
(auto-compression-mode t)
(tooltip-mode nil)
(hl-line-mode t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'sentence-end-double-space nil)

;; UTF-8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; cask/pallet to manage our installed packages.
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; smex.
(require 'smex)
(global-set-key (kbd "M-x") (lambda ()
                              (interactive)
                              (or (boundp 'smex-cache)
                                  (smex-initialize))
                              (global-set-key [(meta x)] 'smex)
                              (smex)))

(global-set-key (kbd "C-c M-x") (lambda ()
                                  (interactive)
                                  (or (boundp 'smex-cache)
                                      (smex-initialize))
                                  (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                  (smex-major-mode-commands)))

;; global hooks.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
(add-hook 'makefile-mode-hook
          'indent-tabs-mode)

;; {wind,frame}move.
(require 'windmove)
(global-set-key [(control H)] 'windmove-left)
(global-set-key [(control J)] 'windmove-down)
(global-set-key [(control K)] 'windmove-up)
(global-set-key [(control L)] 'windmove-right)
(require 'framemove)
(setq framemove-hook-into-windmove t)

;; theme.
(load-theme 'zenburn t)

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

;; save point.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; whitespace settings.
(require 'whitespace)
(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)

;; compile settings.
(require 'compile)
(setq compilation-scroll-output t)

;; save our customizations elsewhere.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; allow flycheck to do its magic with our load-path
(require 'flycheck)
(defun flycheck-emacs-lisp-hook ()
  "Allow flycheck to validate our custom provided elisp files."
  (setq flycheck-emacs-lisp-load-path load-path))
(add-hook 'emacs-lisp-mode-hook #'flycheck-emacs-lisp-hook)

;; load our other setup files.
(add-to-list 'load-path (concat user-emacs-directory "."))
(require 'setup-magit)
(require 'setup-misc)
(require 'setup-dired)
(require 'setup-ido)
(require 'setup-erc)
(require 'setup-eshell)
(require 'setup-org)
(require 'setup-twitter)
(require 'setup-languages)
(require 'setup-session)
(require 'setup-unity)

;; diminish mode ftw
(require 'diminish)
(diminish 'eldoc-mode)
(diminish 'smart-tab-mode)
(diminish 'magit-auto-revert-mode)

;; load only if available
(if (file-exists-p "~/.emacs.d/setup-private.el")
    (require 'setup-private))
(when (require 'mu4e nil 'noerror)
  (require 'setup-mu4e))

;; because there's no other way to run emacs.
(defun toggle-fullscreen ()
  "Always maximize.  Intended for tiling WMs."
  (interactive)
  (if (eq system-type 'windows-nt)
      (when (fboundp 'w32-send-sys-command)
        (w32-send-sys-command 61488))
    (when (fboundp 'x-send-client-message)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))))

;; windows needs this to execute at the end
(when window-system
  (toggle-fullscreen))

;;; init.el ends here
