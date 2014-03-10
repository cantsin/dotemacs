;;; setup-windmove -- Summary
;;; Commentary:
;;; Setup windmove.
;;; Code:
(windmove-default-keybindings)

(global-set-key (kbd "C-c j")  'windmove-left)
(global-set-key (kbd "C-c k") 'windmove-right)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(provide 'setup-windmove)
;;; setup-windmove.el ends here
