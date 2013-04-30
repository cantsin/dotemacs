;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; timestamps in *Messages*
(defun current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))

(defadvice message (before test-symbol activate)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((deactivate-mark nil))
        (save-excursion
          (set-buffer "*Messages*")
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (current-time-microseconds))))))

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
(set-default 'sentence-end-double-space nil)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; not 72, please.
(setq fill-column 80)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Easily navigate sillycased words
(global-subword-mode 1)

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
  '(clojure-mode
    confluence
    diminish
    dired+
    expand-region
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    gnomenm
    haskell-mode
    hl-line+
    ido-ubiquitous
    nrepl
    offlineimap
    openwith
    paredit
    session
    smart-tab
    smex
    twittering-mode
    zenburn-theme

    ;; auxiliary libraries
    dash
    ht
    loop
    s)
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

;; el-get.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)

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

;; remap backspace key to something more sensible
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

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
(require 'setup-eshell)
(require 'setup-mu4e)
(require 'setup-org)
(require 'setup-clojure)
(require 'setup-twitter)
(require 'setup-languages)

;; diminish mode ftw
(require 'diminish)
(diminish 'eldoc-mode)
;(diminish 'paredit-mode "()")
;(diminish 'smart-tab-mode)
