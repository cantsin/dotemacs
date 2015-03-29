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
  (find-file user-init-file))
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
(global-prettify-symbols-mode 1)

;; cask/pallet to manage our installed packages.
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

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

;; save buffers on buffer switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; theme.
;;(load-theme 'zenburn t)
(require 'moe-theme)
(moe-dark)
(moe-theme-set-color 'orange)

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

;; save/restore emacs configuration.
(desktop-save-mode)

;; replace kill-ring-save.
(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)
(define-key easy-kill-base-map (kbd "C-d") 'easy-kill-delete-region)
(define-key easy-kill-base-map (kbd "DEL") 'easy-kill-delete-region)
(add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
(add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
(add-to-list 'easy-kill-alist '(?b buffer ""))
(add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
(add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
(add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
(add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
(add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
(add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))

;; load our other setup files.
(add-to-list 'load-path (concat user-emacs-directory "."))
(require 'setup-fly)
(require 'setup-magit)
(require 'setup-misc)
(require 'setup-dired)
(require 'setup-helm)
(require 'setup-erc)
(require 'setup-eshell)
(require 'setup-org)
(require 'setup-twitter)
(require 'setup-languages)
(require 'setup-session)

;; diminish mode ftw
(require 'diminish)
(diminish 'eldoc-mode)
(diminish 'magit-auto-revert-mode)
(diminish 'projectile-mode " Proj")
(diminish 'company-mode " co")
(diminish 'helm-mode)
(diminish 'abbrev-mode)

;; prettier lines.
(require 'powerline)
(powerline-default-theme)

;; load only if available
(if (file-exists-p "~/.emacs.d/setup-private.el")
    (require 'setup-private))
(when (require 'mu4e nil 'noerror)
  (require 'setup-mu4e))

(defun save-all ()
  "Save whenever focus is lost/gained."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; because there's no other way to run emacs.
(defun toggle-fullscreen ()
  "Always maximize.  Intended for tiling WMs."
  (interactive)
  (toggle-frame-fullscreen))

;; windows needs this to execute at the end
(when window-system
  (toggle-fullscreen))

;;; init.el ends here
