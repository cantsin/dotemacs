;; set up various languages here.
(require 'julia-mode)

(setq auto-mode-alist
      (append '(("\\.jl$" . julia-mode)) auto-mode-alist))

(require 'pretty-mode)
(global-pretty-mode 1)

(provide 'setup-languages)
