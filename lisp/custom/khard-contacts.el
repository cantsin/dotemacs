;;; khard-contacts -- Summary
;;; Commentary:
;;; Minimal company backend for contacts.
;;; Code:
(require 'company)
(require 'json)
(require 'seq)
(require 's)

(defun contacts-retrieve ()
  "Return a list of contacts."
  (let* ((invocation "khard ls -p -F name,emails")
         (output (shell-command-to-string invocation))
         (raw-data (seq-filter (lambda (line) (s-contains? "{" line))
                               (s-lines
                                (s-replace-all '(("'" . "\"")) output)))))
    (seq-reduce (lambda (accum line)
                  (let* ((result (s-split "\t" line t))
                         (name (car result))
                         (emails-json (json-read-from-string (cadr result)))
                         (emails (mapcar (lambda (arr) (s-downcase (aref arr 0)))
                                         (mapcar 'cdr emails-json))))
                    (seq-concatenate 'list accum
                                     (seq-map (lambda (email) (format "%s <%s>" name email)) emails))))
                raw-data '())))

(defun company-contacts-backend (command &optional arg &rest ignored)
  "Company contacts backend. COMMAND ARG IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-contacts-backend))
    (prefix (and (eq major-mode 'notmuch-message-mode)
                 (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
       (lambda (c) (string-prefix-p arg c))
       (contacts-retrieve)))))

(add-to-list 'company-backends 'company-contacts-backend)

(provide 'khard-contacts)
;;; khard-contacts.el ends here
