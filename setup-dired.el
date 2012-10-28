(require 'dired+)

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(provide 'setup-dired)
