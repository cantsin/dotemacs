;;; setup-dired -- Summary
;;; Commentary:
;;; Setup dired.
;;; Code:
(require 'stripe-buffer)
(require 'dired)

(add-hook 'dired-mode-hook 'stripe-listify-buffer)

(setq dired-listing-switches "-alk")

;; Auto refresh dired, but be quiet about it
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(defun dired-back-to-top ()
  "Skip the . and .. directories."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  "Skip the blank line at the end of dired."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(provide 'setup-dired)
;;; setup-dired.el ends here
