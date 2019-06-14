;;; setup-buffer -- summary
;;; Commentary:
;;; set various buffer settings
;;; Code:
(require 'use-package)

(use-package avy
  :demand t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("M-g M-g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package comint
  :config
  (add-to-list 'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output))))

;; replace kill-ring-save.
(use-package easy-kill
  :demand t
  :bind
  (:map easy-kill-base-map ("C-d" . easy-kill-delete-region)
   :map easy-kill-base-map ("DEL" . easy-kill-delete-region)))

(use-package html-mode
  :defer t
  :init
  (add-hook 'html-mode-hook
            '(lambda ()
               (turn-off-auto-fill)
               (visual-line-mode))))

(use-package key-chord
  :demand t
  :config
  (key-chord-mode +1)
  (key-chord-define-global ",," 'ivy-switch-buffer)
  (key-chord-define-global ",." '(lambda ()
                                   (interactive)
                                   (switch-to-buffer (caar (window-prev-buffers)))))
  (key-chord-define-global "jj" 'avy-goto-char))

(use-package ledger
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))

(use-package markdown-mode
  :defer t
  :init
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (font-lock-add-keywords nil '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face)))
               (turn-off-auto-fill)
               (visual-line-mode)
               (writegood-mode)))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  :config
  (defconst markdown-regex-footnote-inline
    "\\(\\^\\[.+?\\]\\)"
    "Regular expression for a footnote inline marker ^[fn].")
  (defface markdown-footnote-inline-face
    '((t (:inherit font-lock-keyword-face)))
    "Face for footnote markers."
    :group 'markdown-faces))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(defmacro def-pairs (pairs)
  `(progn
     ,@(cl-loop for (key . val) in pairs
          collect
            `(defun ,(read (concat
                            "wrap-with-"
                            (prin1-to-string key)
                            "s"))
                 (&optional arg)
               (interactive "p")
               (sp-wrap-with-pair ,val)))))

(use-package smartparens-config
  :demand t
  :ensure smartparens
  :bind (("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)

         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)

         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)

         ("C-<right>" . sp-forward-slurp-sexp)
         ("M-<right>" . sp-forward-barf-sexp)
         ("C-<left>"  . sp-backward-slurp-sexp)
         ("M-<left>"  . sp-backward-barf-sexp)

         ("C-M-t" . sp-transpose-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-k"   . sp-kill-hybrid-sexp)
         ("M-k"   . sp-backward-kill-sexp)
         ("C-M-w" . sp-copy-sexp)

         ("M-[" . sp-backward-unwrap-sexp)
         ("M-]" . sp-unwrap-sexp)

         ("C-x C-t" . sp-transpose-hybrid-sexp)

         ("C-c ("  . wrap-with-parens)
         ("C-c ["  . wrap-with-brackets)
         ("C-c {"  . wrap-with-braces)
         ("C-c '"  . wrap-with-single-quotes)
         ("C-c \"" . wrap-with-double-quotes)
         ("C-c _"  . wrap-with-underscores)
         ("C-c `"  . wrap-with-back-quotes))
  :config
  (def-pairs ((paren        . "(")
              (bracket      . "[")
              (brace        . "{")
              (double-quote . "\"")
              (back-quote   . "`")))
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (show-smartparens-global-mode t))

(use-package pdf-tools
  :defer t
  :init (load "pdf-tools-autoloads" nil t)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package text-mode
  :defer t
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill))

(use-package whitespace
  :defer t
  :init (setq whitespace-style '(face trailing lines-tail tabs)
              whitespace-line-column 80
              global-whitespace-cleanup-mode t))

(use-package windmove
  :demand t
  :bind (("C-S-J" . windmove-left)
         ("C-S-K" . windmove-down)
         ("C-S-L" . windmove-up)
         ([(control :)] . windmove-right))
  :init
  (defadvice windmove-up (before other-window-now activate)
    "Move up."
    (when buffer-file-name (save-buffer)))
  (defadvice windmove-down (before other-window-now activate)
    "Move down."
    (when buffer-file-name (save-buffer)))
  (defadvice windmove-left (before other-window-now activate)
    "Move left."
    (when buffer-file-name (save-buffer)))
  (defadvice windmove-right (before other-window-now activate)
    "Move right."
    (when buffer-file-name (save-buffer))))

(use-package window
  :init
  (defadvice switch-to-buffer (before save-buffer-now activate)
    "Save when switching to buffer."
    (when buffer-file-name (save-buffer)))
  (defadvice other-window (before other-window-now activate)
    "Other window."
    (when buffer-file-name (save-buffer))))

(use-package ws-butler
  :defer t
  :config (ws-butler-global-mode))

(provide 'setup-buffer)
;;; setup-buffer.el ends here
