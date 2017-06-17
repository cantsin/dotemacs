;;; setup-mu4e -- Summary
;;; Commentary:
;;; Setup mu4e.
;;; Code:
(require 'use-package)

(defun cantsin/mu4e-set-account ()
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

(defun archive-email (msg)
  "Archive a MSG."
  (message "archive-email called")
  (let* ((maildir (mu4e-message-field msg :maildir))
         (root (save-match-data
                 (string-match "\\(/[^/]+\\)/.*" maildir)
                 (match-string 1 maildir)))
         (message root)
         (allmail (concat root "/" "Archive")))
    (mu4e-mark-set 'move allmail)))

(defun cantsin/mu4e-config ()
  "Set up mu4e."
  (use-package smtpmail
    :defer t
    :ensure t)
  (use-package mu4e-contrib
    :defer t)
  (add-hook 'mu4e-compose-pre-hook 'cantsin/mu4e-set-account)
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (defvar my-mu4e-account-alist 'nil)
  (add-to-list 'mu4e-headers-actions
               '("Archive email" . archive-email) t)
  (add-to-list 'mu4e-headers-actions
               '("in browser" . mu4e-action-view-in-browser) t)
  (fset 'my-move-to-archive "aA")
  (define-key mu4e-headers-mode-map (kbd "e")
    'my-move-to-archive)
  (setq message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t
        mu4e-html2text-command 'mu4e-shr2text
        mu4e-sent-messages-behavior 'delete
        mu4e-use-fancy-chars t
        mu4e-headers-leave-behavior 'apply
        mu4e-confirm-quit nil
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 3600))

(use-package mu4e
  :defer t
  :config (cantsin/mu4e-config))

(provide 'setup-mu4e)
;;; setup-mu4e.el ends here
