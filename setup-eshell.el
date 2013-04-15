(require 'eshell)
(require 'em-smart)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(defun eshell/emacs (file)
  (find-file file))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/magit ()
  "Run magit status here."
  (call-interactively #'magit-status)
  nil)

(defun eshell/gd ()
  "Run magit diff here."
  (call-interactively #'magit-diff-working-tree)
  nil)

;; args does not seem to consume arguments?
;; eshell-command does not do what we want?
;; def-advice?
(defun eshell/git (command &rest args)
  (cond
   ((> (length args) 0)
    (apply 'eshell-command (cons (concat "git-" command) args)))
   ((equal command "diff")
    (call-interactively #'magit-diff-working-tree))
   ((equal command "status")
    (call-interactively #'magit-status))
   (t
    (apply 'eshell-command (cons (concat "git-" command) args)))))

(defun eshell/deb (&rest args)
  (eshell-eval-using-options
   "deb" args
   '((?f "find" t find "list available packages matching a pattern")
     (?i "installed" t installed "list installed debs matching a pattern")
     (?l "list-files" t list-files "list files of a package")
     (?s "show" t show "show an available package")
     (?v "version" t version "show the version of an installed package")
     (?w "where" t where "find the package containing the given file")
     (nil "help" nil nil "show this usage information")
     :show-usage)
   (eshell-do-eval
    (eshell-parse-command
     (cond
      (find
       (format "apt-cache search %s" find))
      (installed
       (format "dlocate -l %s | grep '^.i'" installed))
      (list-files
       (format "dlocate -L %s | sort" list-files))
      (show
       (format "apt-cache show %s" show))
      (version
       (format "dlocate -s %s | egrep '^(Package|Status|Version):'" version))
      (where
       (format "dlocate %s" where))))
    t)))

(provide 'setup-eshell)
