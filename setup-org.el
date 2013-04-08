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

(require 'stripe-buffer)

(add-hook 'org-mode-hook 'org-table-stripes-enable)

(setq org-startup-indented t)
(setq org-archive-location ".archived.org::* From %s")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(provide 'setup-org)
