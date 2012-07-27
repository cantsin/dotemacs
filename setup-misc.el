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
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; smart-tab.
(require 'smart-tab)
(add-hook 'emacs-lisp-mode-hook 'smart-tab-mode)

(provide 'setup-misc)
