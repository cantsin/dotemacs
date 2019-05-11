;;; setup-ivy -- Summary
;;; Commentary:
;;; Setup ivy.
;;; Code:
(require 'use-package)

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map ("M-y" . ivy-next-line)
   :map ivy-minibuffer-map ("C-o" . hydra-ivy/body)
   :map ivy-mode-map ("C-'" . ivy-avy)))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-height 18
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(use-package counsel
  :bind
  (:map counsel-find-file-map ("C-l" . counsel-up-directory)))

(use-package all-the-icons-ivy
  :ensure t
  :config
  (progn
    (all-the-icons-ivy-setup)
    (setq all-the-icons-ivy-file-commands
          '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))))

(provide 'setup-ivy)
;;; setup-ivy.el ends here
