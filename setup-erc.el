(require 'erc)
(require 'erc-match)
(require 'erc-join)
(require 'erc-fill)
(require 'erc-ring)
(require 'erc-netsplit)
(require 'tls)

(erc-spelling-mode 1)
(erc-autojoin-mode 1)
(erc-match-mode)
(erc-fill-mode t)
(erc-ring-mode t)
(erc-netsplit-mode t)
(erc-timestamp-mode t)

(setq erc-timestamp-format "[%D %r]")

;; logging
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irc_logs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)
(setq erc-auto-query 'buffer)

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

;; add ERC to the menu.
(require 'easymenu)
(easy-menu-add-item nil '("tools") ["IRC with ERC" erc t])

;; custom command.
(defun irc ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667)
    (erc-tls :server "irc.flowdock.com" :port 6697)))

(provide 'setup-erc)
