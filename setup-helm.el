;;; setup-helm -- Summary
;;; Commentary:
;;; Setup helm.
;;; Code:
(require 'use-package)

;; helm with ag
(defun projectile-helm-ag ()
  "Use projectile with helm-ag."
  (interactive)
  (helm-do-ag (projectile-project-root)))
(global-set-key (kbd "C-c h s") 'projectile-helm-ag)
(custom-set-variables
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point 'symbol))

(defun helm-toggle-header-line ()
  "Toggle the header line."
  (if (= (length helm-sources) 1)
      (set-face-attribute 'helm-source-header nil :height 0.1)
    (set-face-attribute 'helm-source-header nil :height 1.0)))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-unset-key (kbd "C-x c"))

(defun cantsin/helm-setup ()
  "Set up helm."
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))
  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
  (projectile-global-mode)
  (helm-projectile-on)
  (helm-mode 1))

(defun cantsin/helm-config ()
  "Configure helm."
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
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
