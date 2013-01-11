(require 'org-bullets)

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
