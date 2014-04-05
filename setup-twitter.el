;;; setup-twitter -- summary
;;; Commentary:
;;; set twitter defaults.

;;; Code:
(require 'twittering-mode)

(setq twittering-cert-file "/etc/ssl/certs/ca-certificates.crt")
(setq twittering-use-master-password t)
(setq twittering-url-show-status nil)

(add-hook 'twittering-mode
          '(lambda () (visual-line-mode)))

(provide 'setup-twitter)
;;; setup-twitter.el ends here
