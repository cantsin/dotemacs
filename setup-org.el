;;; setup-org -- Summary
;;; Commentary:
;;; Setup org-mode.
;;; Code:
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

;; (defadvice org-archive-subtree (after fix-cookies activate)
;;   (update-parent-cookie))

(require 'stripe-buffer)

(add-hook 'org-mode-hook 'org-table-stripes-enable)

(setq org-startup-indented t)
(setq org-return-follows-link t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-use-fast-todo-selection t)
(setq org-archive-location ".archived.org::* From %s")
(setq org-default-notes-file "~/remote/Dropbox/notes.org")
(setq org-stuck-projects
      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("r" "respond" entry (file org-default-notes-file)
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n\n")))
(setq org-todo-keywords
      '((sequence "TODO(t)" "PENDING(p)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "FUTURE(l)")
        (sequence "|" "CANCELED(c)")))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ce" 'org-archive-subtree)

;; set up shortcut for priorities
(push '("p" "Agenda for all priorities" agenda ""
        ((org-agenda-skip-function
          '(and
            (not
             (org-entry-get nil "PRIORITY"))
            (point-at-eol)))))
      org-agenda-custom-commands)

(add-hook 'org-mode-hook
          #'(lambda () (setq electric-indent-mode nil)))

(provide 'setup-org)
;;; setup-org.el ends here
