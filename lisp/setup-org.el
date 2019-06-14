;;; setup-org -- Summary
;;; Commentary:
;;; Setup org-mode.
;;; Code:
(require 'use-package)

(defun update-parent-cookie ()
  "Update the count of items in this section."
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  "When killing the line, update the cookie."
  (update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  "When killing the whole line, also update the cookie."
  (update-parent-cookie))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("M-p" . org-previous-visible-heading)
         ("M-n" . org-next-visible-heading)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c e" . org-archive-subtree))
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (setq electric-indent-mode nil)
              (org-update-statistics-cookies t)))
  (add-hook 'org-mode-hook 'org-table-stripes-enable)
  :config
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-default-notes-file "~/todos.org/notes.org"
        org-agenda-files '("~/todos.org/")
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-startup-indented t
        org-return-follows-link t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-use-fast-todo-selection t
        org-archive-location ".archived.org::* From %s"
        org-columns-default-format "%60ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
        org-todo-keywords
        '((sequence "TODO(t)" "PENDING(p)" "|" "DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
          (sequence "|" "FUTURE(l)")
          (sequence "|" "CANCELED(c)" "BLOCKED(x)")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t))))

(use-package org-agenda
  :defer t
  :config
  (setq org-agenda-start-on-weekday 6
        org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:")
        ;; set up shortcut for priorities
        org-agenda-custom-commands
        '(("p" "Agenda for all priorities" agenda ""
           ((org-agenda-skip-function
             '(and
               (not
                (org-entry-get nil "PRIORITY"))
               (point-at-eol))))))))

(use-package org-capture
  :defer t
  :config (setq org-capture-templates
                '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
                   "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                  ("r" "respond" entry (file org-default-notes-file)
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n\n"))
                ))

(use-package org-bullets
  :defer t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-notmuch
  :defer t)

(use-package org-journal
  :defer t
  :config
  (setq org-journal-dir "~/journal"))

(use-package stripe-buffer
  :defer t)

(provide 'setup-org)
;;; setup-org.el ends here
