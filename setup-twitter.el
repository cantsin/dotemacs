(require 'twittering-mode)

(setq twittering-use-master-password t)
(setq twittering-url-show-status nil)

(add-hook 'twittering-mode
          '(lambda () (longlines-mode)))

(provide 'setup-twitter)
