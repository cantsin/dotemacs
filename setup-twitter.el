;;; setup-twitter -- summary
;;; Commentary:
;;; set twitter defaults.

;;; Code:
(require 'use-package)

(defun cantsin/twitter-setup ()
  "Setup twitter."
  (setq twittering-cert-file "/etc/ssl/certs/ca-certificates.crt"
        twittering-use-master-password t
        twittering-url-show-status nil
        twittering-tinyurl-service 'bit.ly)
  (add-hook 'twittering-mode
            '(lambda () (visual-line-mode))))

(use-package twittering-mode
  :ensure t
  :defer t
  ;; swap p,n with j,k
  ;; :bind (:map twittering-mode-map
  ;;             (("n" . twittering-goto-next-status)
  ;;              ("p" . twittering-goto-previous-status)
  ;;              ("j" . twittering-goto-next-status-of-user)
  ;;              ("k" . twittering-goto-previous-status-of-user)))
  :config (cantsin/twitter-setup))

(provide 'setup-twitter)
;;; setup-twitter.el ends here
