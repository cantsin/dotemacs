(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(erc-log-mode t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp spelling track)))
 '(ledger-reports
   (quote
    (("equity" "ledger equity")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(mu4e-view-show-images t)
 '(org-agenda-file-regexp "\\`[^._].*\\.org\\'")
 '(org-default-priority 53)
 '(org-highest-priority 49)
 '(org-log-done (quote time))
 '(org-lowest-priority 57)
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 122 :width normal))))
 '(agda2-highlight-datatype-face ((t (:foreground "yellow"))))
 '(agda2-highlight-function-face ((t (:foreground "gold"))))
 '(agda2-highlight-primitive-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "yellow"))))
 '(agda2-highlight-record-face ((t (:foreground "deep sky blue"))))
 '(flyspell-duplicate ((t (:inherit warning :foreground "pale turquoise" :weight bold))))
 '(markdown-blockquote-face ((t (:background "gray19" :foreground "#ff8700" :slant italic))))
 '(markdown-inline-code-face ((t (:background "gray19" :foreground "#5fafd7"))))
 '(stripe-hl-line ((t (:background "dark violet" :foreground "unspecified-bg")))))
