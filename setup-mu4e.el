;;; setup-mu4e -- Summary
;;; Commentary:
;;; Setup mu4e.
;;; Code:
(require 'mu4e)

;; set up "main" account first
(setq mu4e-maildir "~/remote/email")

;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
(setq message-kill-buffer-on-exit t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-view-show-images t)
(setq mu4e-headers-leave-behavior 'apply)
(setq mu4e-confirm-quit nil)

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval 300)

(defun archive-email (msg)
  (let* ((maildir (mu4e-message-field msg :maildir))
         (root (save-match-data
                 (string-match "\\(/[^/]+\\)/.*" maildir)
                 (match-string 1 maildir)))
         (allmail (concat root "/" "[Gmail].All Mail")))
    (mu4e-mark-set 'move allmail)))

(add-to-list 'mu4e-headers-actions
             '("Archive email" . archive-email) t)

(fset 'my-move-to-archive "AA")

(define-key mu4e-headers-mode-map (kbd "a")
  'my-move-to-archive)

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(add-hook 'mu4e-view-mode-hook
          '(lambda () (progn
                        ;(longlines-mode)
                        (mu4e-view-toggle-hide-cited))))

;; this seems better than the default html2text
;; pip install html2text
(setq mu4e-html2text-command "python -m html2text")

(provide 'setup-mu4e)
;;; setup-mu4e.el ends here
