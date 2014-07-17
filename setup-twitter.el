;;; setup-twitter -- summary
;;; Commentary:
;;; set twitter defaults.

;;; Code:
(require 'twittering-mode)

(setq twittering-cert-file "/etc/ssl/certs/ca-certificates.crt")
(setq twittering-use-master-password t)
(setq twittering-url-show-status nil)

(setq twittering-tinyurl-service 'bit.ly)

;; swap p,n with j,k
(define-key twittering-mode-map (kbd "n") 'twittering-goto-next-status)
(define-key twittering-mode-map (kbd "p") 'twittering-goto-previous-status)
(define-key twittering-mode-map (kbd "j") 'twittering-goto-next-status-of-user)
(define-key twittering-mode-map (kbd "k") 'twittering-goto-previous-status-of-user)

(add-hook 'twittering-mode
          '(lambda () (visual-line-mode)))

(provide 'setup-twitter)
;;; setup-twitter.el ends here
