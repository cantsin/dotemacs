;;; setup-buffer -- summary
;;; Commentary:
;;; set various buffer settings
;;; Code:
(require 'use-package)

;; wrapping.
(add-hook 'text-mode-hook
          'turn-on-auto-fill)
(add-hook 'html-mode-hook
          '(lambda ()
             (turn-off-auto-fill)
             (visual-line-mode)))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (turn-off-auto-fill)
             (visual-line-mode)))

;;;; NB this breaks latest helm so disabling for now
;; turn off annoying ffap behavior
;; (use-package ffap
;;   :defer t
;;   :config (setq ffap-alist nil
;;                 ffap-machine-p-known 'accept
;;                 ffap-require-prefix nil
;;                 ffap-gopher-regexp nil
;;                 ffap-url-regexp nil
;;                 ffap-ftp-regexp nil
;;                 ffap-ftp-sans-slash-regexp nil
;;                 ffap-rfs-regexp nil
;;                 ffap-shell-prompt-regexp nil))
;; (defun ffap-file-at-point nil
;;   "Turn off ffap file-at-point completely."
;;   nil)

(use-package windmove
  :ensure t)

(use-package framemove
  :ensure t
  :config (setq framemove-hook-into-windmove t))

;; save buffers on buffer switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  "Switch to buffer."
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  "Other window."
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  "Move up."
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  "Move down."
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  "Move left."
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  "Move right."
  (when buffer-file-name (save-buffer)))

;; save point.
(use-package saveplace
  :ensure t
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (concat user-emacs-directory "places"))))

;; save/restore emacs configuration.
(use-package desktop
  :ensure t
  :init (desktop-save-mode))

(defun bjm-deft-save-windows (orig-fun &rest args)
  "Advice to save windows -- ORIG-FUN ARGS."
  (setq bjm-pre-deft-window-config (current-window-configuration))
  (apply orig-fun args))

(defun bjm-quit-deft ()
  "Save buffer, kill buffer, kill deft buffer, and restore window config to the way it was before deft was invoked."
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  (switch-to-buffer "*Deft*")
  (kill-this-buffer)
  (when (window-configuration-p bjm-pre-deft-window-config)
    (set-window-configuration bjm-pre-deft-window-config)))

(use-package deft
  :init (advice-add 'deft :around #'bjm-deft-save-windows)
  :bind (("C-c q" . bjm-quit-deft)
         ("C-c d" . deft-new-file)))

;; markdown modifications
(require 'markdown-mode)
(defconst markdown-regex-footnote-inline
  "\\(\\^\\[.+?\\]\\)"
  "Regular expression for a footnote inline marker ^[fn].")
(defface markdown-footnote-inline-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'markdown-faces)
(add-to-list 'markdown-mode-font-lock-keywords-basic
             (cons markdown-regex-footnote-inline 'markdown-footnote-face))

;; TODO: set to white
(defun add-quotes-to-font-lock-keywords ()
  "In markdown, set quote font lock."
  (font-lock-add-keywords nil '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face))))
(add-hook 'markdown-mode-hook 'add-quotes-to-font-lock-keywords)

(use-package avy
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("s-y" . ace-window)))

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(use-package pdf-tools
  :config (pdf-tools-install))

(provide 'setup-buffer)
;;; setup-buffer.el ends here
