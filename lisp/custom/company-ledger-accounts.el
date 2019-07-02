;;; company-ledger-accounts -- Summary
;;; Commentary:
;;; Minimal company backend for ledger-mode.
;;; Code:
(require 'company)

(defun company-ledger-accounts-retrieve (string)
  "Return a list of accounts contained in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match "^account \\(.*\\)" string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun company-ledger-accounts-backend (command &optional arg &rest ignored)
  "Company ledger accounts backend. COMMAND ARG IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ledger-accounts-backend))
    (prefix (and (eq major-mode 'ledger-mode)
                 (or (company-grab-line "^    \\(.+\\)" 1) 'stop)))
    (candidates
     (cl-remove-if-not
       (lambda (c) (string-prefix-p arg c))
       (company-ledger-accounts-retrieve (buffer-string))))))

(add-to-list 'company-backends 'company-ledger-accounts-backend)

(provide 'company-ledger-accounts)
;;; company-ledger-accounts.el ends here
