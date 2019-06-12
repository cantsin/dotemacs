;;; setup-dired -- Summary
;;; Commentary:
;;; Setup dired.
;;; Code:
(require 'use-package)

;; Auto refresh dired, but be quiet about it
(use-package autorevert
  :demand t)

(use-package all-the-icons-dired
  :demand t
  :hook (dired-mode . all-the-icons-dired-mode))

(defun dired-back-to-top ()
  "Skip the . and .. directories."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  "Skip the blank line at the end of dired."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(use-package dired
  :demand t
  :hook (dired-mode . stripe-listify-buffer)
  :bind
  (:map dired-mode-map ([remap beginning-of-buffer] . dired-back-to-top)
        :map dired-mode-map ([remap end-of-buffer] . dired-jump-to-bottom))
  :config
  (setq dired-listing-switches "-alhv"
        dired-dwim-target t
        dired-clean-up-buffers-too t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(require 'dired-x)

(use-package stripe-buffer
  :demand t
  :config (progn
            (set-face-background 'stripe-hl-line "dark violet")
            (set-face-foreground 'stripe-hl-line "white")))

(provide 'setup-dired)
;;; setup-dired.el ends here
