(require 'org-bullets)

(add-hook 'org-mode-hook
          (lambda () (org-bullets-mode 1)))

(require 'stripe-buffer)

(add-hook 'org-mode-hook 'org-table-stripes-enable)

(setq org-startup-indented t)
(setq org-archive-location ".archived.org::* From %s")

(provide 'setup-org)
