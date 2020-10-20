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
  ;; (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  :config
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Valkyrie T4" :height 180 :weight thin))))
   '(fixed-pitch ((t (:family "Triplicate T3c" :height 180))))
   ;; TODO
   ;; '(org-link ((t (:inherit variable-pitch :height 1.3))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-block-begin-line ((t (:inherit fixed-pitch))))
   '(org-block-end-line ((t (:inherit fixed-pitch))))
   '(org-drawer ((t (:inherit fixed-pitch))))
   '(org-date ((t (:inherit fixed-pitch))))
   '(org-property-value ((t (:inherit fixed-pitch))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-special-keyword ((t (:inherit fixed-pitch))))
   '(org-checkbox ((t (:inherit fixed-pitch))))
   '(org-priority ((t (:inherit fixed-pitch))))
   '(org-meta-line ((t (:inherit fixed-pitch))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-todo ((t (:inherit fixed-pitch))))
   '(org-done ((t (:inherit fixed-pitch))))
   '(org-priority ((t (:inherit fixed-pitch)))))
  (let* ((variable-tuple '(:font "Valkyrie T3"))
         (base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit default :weight normal :foreground ,base-font-color)))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
  (setq org-default-notes-file "~/todos.org/notes.org"
        org-agenda-files '("~/todos.org/")
        org-confirm-babel-evaluate nil
        org-image-actual-width nil
        org-hide-emphasis-markers t
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
  :config (setq org-capture-templates
                '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
                   "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
                  ("r" "respond" entry (file org-default-notes-file)
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n\n"))
                ))

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-clock-convenience
  :demand t
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("<S-home>" . org-clock-convenience-fill-gap)
              ("<S-end>" . org-clock-convenience-fill-gap-both))
  :config
  ;; make clocked times show even if archived
  (setq org-agenda-archives-mode t)
  (setq org-clock-clocktable-default-properties '(:maxlevel 2 :scope agenda-with-archives)))

(use-package org-journal
  :custom
  (org-journal-dir "~/journal"))

(use-package stripe-buffer
  :defer t)

(provide 'setup-org)
;;; setup-org.el ends here
