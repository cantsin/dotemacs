;;; setup-theme -- Summary
;;; Commentary:
;;; Setup theme modifications.
;;; Code:
(require 'use-package)
(require 'subr-x)

;; if icons do not show up:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :demand t)

(setq-default frame-title-format
              '(:eval
                (format "emacs %s"
                        (cond
                         (buffer-file-truename
                          buffer-file-truename)
                         (dired-directory
                          (concat "[" dired-directory "]"))
                         (t
                          (concat "(" (string-trim (buffer-name)) ")"))))))

(use-package moe-theme
  :demand t
  :config
  (moe-dark))

(use-package auto-dim-other-buffers
  :demand t
  :config
  (auto-dim-other-buffers-mode t)
  (set-face-background 'auto-dim-other-buffers-face "gray18"))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-height 1)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq mode-line-percent-position nil)
  (set-face-background 'mode-line "gray40")
  (set-face-background 'mode-line-buffer-id "gray40")
  (set-face-background 'mode-line-inactive "gray40")
  (set-face-foreground 'mode-line "white")
  (set-face-foreground 'mode-line-buffer-id "white")
  (set-face-foreground 'mode-line-inactive "gray20")
  (set-face-attribute 'mode-line-highlight nil :box nil)
  :config (doom-modeline-mode 1))

(provide 'setup-theme)
;;; setup-theme.el ends here
