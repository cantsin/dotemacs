;; this should be already part of emacs.
(defun copy-line (&optional arg)
  (interactive "P")
  (save-excursion
    (toggle-read-only 1)
    (kill-line arg)
    (toggle-read-only 0)))

(setq-default kill-read-only-ok t) ;; required for copy-line.
(global-set-key "\C-c\C-k" 'copy-line)

;; save our point position between sessions elsewhere.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/save-place/")

;; default hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; paredit.
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
;; make paredit and eldoc play nice.
(eldoc-add-command 'paredit-backward-delete 'paredit-close-round)

;; smart-tab.
(require 'smart-tab)
(add-hook 'emacs-lisp-mode-hook 'smart-tab-mode)

;; uniquify buffers.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; diff.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq diff-switches "-u")

;; winner-mode.
(winner-mode 1)

;; paren-mode.
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

;; expand-mode.
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; enable some disabled commands.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; pretty-print evals.
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(global-set-key [remap eval-expression] 'pp-eval-expression)

(provide 'setup-misc)
