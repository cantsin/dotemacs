;;; setup-go -- Summary
;;; Commentary:
;;; Setup for go.
;;; Code:
(require 'use-package)

;; for external tools, need to install:
;; go get code.google.com/p/rog-go/exp/cmd/godef
;; go get code.google.com/p/go.tools/cmd/godoc
(defun cantsin/setup-go ()
  "Set up go."
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-f") 'gofmt)))
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-k") 'godoc))))

(use-package go-mode
  :defer t
  :ensure t
  :init (cantsin/setup-go))

;;; setup-go.el ends here
