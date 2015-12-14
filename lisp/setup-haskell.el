;;; setup-haskell -- Summary
;;; Commentary:
;;; Setup for Haskell and related.
;;; Code:
(require 'use-package)

(use-package purscheck
  :defer t
  :config (add-to-list 'load-path "purscheck.el")) ;; custom

(defun cantsin/setup-purescript ()
  "Set up purescript."
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  (add-hook 'purescript-mode-hook 'turn-on-purescript-unicode-input-method)
  (define-key purescript-mode-map (kbd "C-c C-c") 'purescript-compile)
  (define-key purescript-mode-map (kbd "C-x C-d") nil)
  (define-key purescript-mode-map (kbd "C-c C-z") 'purescript-interactive-switch)
  (define-key purescript-mode-map (kbd "C-c C-l") 'purescript-process-load-file)
  (define-key purescript-mode-map (kbd "C-c C-b") 'purescript-interactive-switch)
  (define-key purescript-mode-map (kbd "C-c C-t") 'purescript-process-do-type)
  (define-key purescript-mode-map (kbd "C-c C-i") 'purescript-process-do-info)
  (define-key purescript-mode-map (kbd "C-c M-.") nil)
  (define-key purescript-mode-map (kbd "C-c C-d") nil)
  (load "purescript-mode-autoloads"))

(use-package purescript-mode
  :defer t
  :ensure t
  :config (cantsin/setup-purescript))

(defun flymake-haskell-init ()
  "Generate a tempfile, run `hslint` on it, and delete file."
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "hslint" (list local-file))))

(defun flymake-haskell-enable ()
  "Enables 'flymake-mode' for haskell."
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name))
    (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
    (flymake-mode t)))

(defun cantsin/setup-haskell ()
  "Set up haskell."
  (use-package haskell-interactive-mode)
  (use-package haskell-process)
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-tags-on-save t)
   '(haskell-process-type 'cabal-repl))
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (eval-after-load 'haskell-mode
    '(progn
       (require 'flymake)
       (push '("\\.l?hs\\'" flymake-haskell-init) flymake-allowed-file-name-masks)
       (add-hook 'haskell-mode-hook 'flymake-haskell-enable)))
  (eval-after-load 'haskell-mode
    '(progn
       (load-library "inf-haskell")
       (defun my-inf-haskell-hook ()
         (setq comint-prompt-regexp
               (concat comint-prompt-regexp "\\|^.> ")))
       (add-to-list 'inferior-haskell-mode-hook 'my-inf-haskell-hook))))

(use-package haskell-mode
  :ensure t
  :defer t
  :config (cantsin/setup-haskell))

(provide 'setup-haskell)
;;; setup-haskell.el ends here