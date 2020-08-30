;;; setup-ivy -- Summary
;;; Commentary:
;;; Setup ivy.
;;; Code:
(require 'use-package)

(use-package ivy
  :demand t
  :bind
  (:map ivy-minibuffer-map ("M-y" . ivy-next-line)
   :map ivy-minibuffer-map ("C-w" . ivy-yank-word)
   :map ivy-minibuffer-map ("C-o" . hydra-ivy/body)
   :map ivy-minibuffer-map ("C-r" . ivy-previous-line)
   :map ivy-mode-map ("C-'" . ivy-avy))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-height 18
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        enable-recursive-minibuffers t))

(use-package all-the-icons-ivy
  :demand t
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file
          counsel-file-jump
          counsel-recentf
          counsel-projectile-find-file
          counsel-projectile-find-dir)))

(use-package ivy-rich
  :demand t
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    "Returns an icon for the candidate out of `all-the-icons'."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.9)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.9)
          icon))))
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon :width 2)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
          ))
  (ivy-rich-mode 1))

(use-package counsel
  :demand t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop))
  (:map counsel-find-file-map ("C-l" . counsel-up-directory)))

(use-package counsel-projectile
  :demand t
  :ensure counsel
  :bind (("M-g s" . counsel-projectile-rg))
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(provide 'setup-ivy)
;;; setup-ivy.el ends here
