;;; setup-ivy -- Summary
;;; Commentary:
;;; Setup ivy.
;;; Code:
(require 'use-package)

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)
   :map ivy-mode-map
   ("C-'" . ivy-avy)))
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
  (("C-l" . counsel-up-directory)
   :map counsel-find-file-map))

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(provide 'setup-ivy)
;;; setup-ivy.el ends here
