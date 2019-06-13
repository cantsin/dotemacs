;;; setup-lisp -- Summary
;;; Commentary:
;;; Setup for various lisps.
;;; Code:
(require 'use-package)

(use-package cider
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-mode-hook 'subword-mode)
  (add-hook 'cider-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (setq cider-show-error-buffer nil)
  (add-to-list 'same-window-buffer-names "*cider*"))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(use-package elisp
  :defer t
  :bind (("C-c v" . eval-buffer)
         ("C-c C-e" . eval-and-replace))
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package elisp-slime-nav
  :defer t
  :config (add-hook 'emacs-lisp-mode-hook
                    (lambda () (elisp-slime-nav-mode t))))

(use-package eldoc
  :defer t)

(use-package paren
  :ensure t
  :init
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (setq show-paren-style 'mixed))

(use-package paredit
  :defer t
  :config
  ;; make paredit and eldoc play nice.
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(provide 'setup-lisp)
;;; setup-lisp.el ends here
