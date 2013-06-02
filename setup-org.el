(require 'org-bullets)

(setq org-use-speed-commands t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(add-hook 'org-mode-hook
          (lambda () (org-bullets-mode 1)))
(add-hook 'org-after-todo-state-change-hook
          (lambda () (org-update-statistics-cookies t)))

(defun update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (update-parent-cookie))

(require 'stripe-buffer)

(add-hook 'org-mode-hook 'org-table-stripes-enable)

(setq org-startup-indented t)
(setq org-archive-location ".archived.org::* From %s")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ce" 'org-archive-subtree)

(add-hook 'org-mode-hook
          #'(lambda () (setq electric-indent-mode nil)))

(provide 'setup-org)
