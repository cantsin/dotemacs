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
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
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
 '(package-selected-packages
   (quote
    (scala-mode zop-to-char znc yaml-mode writegood-mode whitespace-cleanup-mode wgrep web-mode virtualenvwrapper viking-mode use-package typo twittering-mode tuareg toml-mode tagedit synonyms swiper-helm stripe-buffer solarized-theme smartparens smart-compile skewer-mode session scss-mode rust-mode restclient rainbow-delimiters purescript-mode protobuf-mode prodigy pretty-mode powerline pdf-tools paredit paradox pandoc-mode pallet org-journal org-bullets ocodo-svg-modelines ob-ipython notmuch nodejs-repl multiple-cursors moe-theme markdown-mode+ magit-gh-pulls lua-mode ledger-mode kv key-chord jsx-mode json-mode js3-mode js-comint jedi hl-line+ helm-swoop helm-spaces helm-projectile helm-make helm-hoogle helm-gtags helm-git-grep helm-dash helm-company glsl-mode gitignore-mode gitconfig ggtags fsharp-mode framemove flymake-lua flycheck-rust flycheck-haskell flycheck-color-mode-line flycheck-cask expand-region esxml esup eshell-prompt-extras erc-image ember-yasnippets ember-mode elm-mode elixir-yasnippets elisp-slime-nav elfeed-org easy-kill-extras dockerfile-mode deft csharp-mode company-go company-ghc clojure-mode-extra-font-locking cider caskxy bbdb babel-repl avy autodisass-llvm-bitcode auto-yasnippet auto-complete-rst auctex ascii arduino-mode alchemist ace-jump-mode)))
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
