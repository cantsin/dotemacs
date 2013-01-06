(require 'stripe-buffer)

(add-hook 'dired-mode-hook 'stripe-listify-buffer)

(setq dired-listing-switches "-alk")

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(require 'dired+)

;; (add-hook 'dired-load-hook
;;          (function (lambda () (load "dired-x"))))

(provide 'setup-dired)
