;;; setup-haskell -- Summary
;;; Commentary:
;;; Setup for Haskell and related.
;;; Code:
(require 'use-package)

(use-package haskell-mode
  :defer t
  :bind
  (:map haskell-mode-map ("C-c C-l" . haskell-process-load-or-reload)
   :map haskell-mode-map ("C-`" . haskell-interactive-bring)
   :map haskell-mode-map ("C-c C-t" . haskell-process-do-type)
   :map haskell-mode-map ("C-c C-i" . haskell-process-do-info)
   :map haskell-mode-map ("C-c C-c" . haskell-process-cabal-build)
   :map haskell-mode-map ("C-c C-k" . haskell-interactive-mode-clear)
   :map haskell-mode-map ("C-c c" . haskell-process-cabal)
   :map haskell-mode-map ("SPC" . haskell-mode-contextual-space)
   :map haskell-mode-map ("M-." . haskell-mode-jump-to-def-or-tag)
   :map haskell-cabal-mode-map ("C-`" . haskell-interactive-bring)
   :map haskell-cabal-mode-map ("C-c C-k" . haskell-interactive-mode-clear)
   :map haskell-cabal-mode-map ("C-c C-c" . haskell-process-cabal-build)
   :map haskell-cabal-mode-map ("C-c c" . haskell-process-cabal))
  :config
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-tags-on-save t)
   '(haskell-process-type 'cabal-repl))
  (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package haskell-interactive-mode
  :defer t)

(use-package haskell-process
  :defer t)

(provide 'setup-haskell)
;;; setup-haskell.el ends here
