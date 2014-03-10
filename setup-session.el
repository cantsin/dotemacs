;;; setup-session -- Summary
;;; Commentary:
;;; Setup session.
;;; Code:
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(require 'org)
(defun maybe-reveal ()
  "Reveal contents depending on mode."
  (when (and (or (memq major-mode '(org-mode outline-mode))
                 (and (boundp 'outline-minor-mode)
                      outline-minor-mode))
             (outline-invisible-p))
    (if (eq major-mode 'org-mode)
        (org-reveal)
      (show-subtree))))

;; fix org oddities
(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (add-hook 'session-after-jump-to-last-change-hook 'maybe-reveal)
  (add-to-list 'session-globals-exclude 'org-mark-ring))

(provide 'setup-session)
;;; setup-session.el ends here
