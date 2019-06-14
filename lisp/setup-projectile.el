;;; setup-projectile -- Summary
;;; Commentary:
;;; Setup projectile.
;;; Code:
(require 'use-package)

(use-package projectile
  :demand t
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (projectile-global-mode)
  (setq projectile-switch-project-action 'projectile-find-file-dwim
        projectile-completion-system 'ivy
        projectile-switch-project-action 'counsel-projectile-find-file
        projectile-use-git-grep t))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
