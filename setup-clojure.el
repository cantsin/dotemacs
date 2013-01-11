
(setq nrepl-popup-stacktraces nil)
(setq nrepl-lein-command "~/bin/lein")

(add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

(provide 'setup-clojure)
