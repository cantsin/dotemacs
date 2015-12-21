;;; setup-helm -- Summary
;;; Commentary:
;;; Setup helm.
;;; Code:
(require 'use-package)

(defun helm-toggle-header-line ()
  "Toggle the header line."
  (if (= (length helm-sources) 1)
      (set-face-attribute 'helm-source-header nil :height 0.1)
    (set-face-attribute 'helm-source-header nil :height 1.0)))

(defun cantsin/helm-setup ()
  "Set up helm."
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))
  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
  (projectile-global-mode)
  (helm-projectile-on)
  (helm-mode 1))

(defun cantsin/helm-config ()
  "Configure helm."
  (use-package helm-swoop)
  (use-package helm-spaces)
  (use-package helm-config)
  (use-package helm-eshell)
  (use-package helm-gtags)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-spaces-new-space-query nil
        helm-multi-swoop-edit-save t
        helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-speed-or-color nil
        helm-swoop-use-line-number-face t
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile
        helm-projectile-sources-list '(helm-source-projectile-projects
                                       helm-source-projectile-files-list)
        projectile-use-git-grep t
        helm-quick-update t
        helm-split-window-in-side-p t
        helm-buffers-fuzzy-matching t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t))

(use-package helm
  :ensure t
  :defer t
  :diminish ""
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c h" . helm-command-prefix)
         ("C-c h SPC" . helm-all-mark-rings)
         ("C-c h o" . helm-occur)
         ("C-c h x" . helm-register)
         ("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         ("C-c h SPC" . helm-spaces))
  :config (cantsin/helm-config)
  :init (cantsin/helm-setup))

(provide 'setup-helm)
;;; setup-helm.el ends here
