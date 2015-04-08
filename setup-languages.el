;;; setup-languages -- Summary
;;; Commentary:
;;; Setup for various languages.
;;; Code:
(require 'use-package)

;; indentation.
(require 'js)
(setq js-indent-level 2)
(setq c-basic-offset 4)
(c-set-offset 'substatement-open 0)

(require 'lua-mode)
(setq lua-indent-level 2)

;; auto disassemble llvm when opening .bc files
(require 'autodisass-llvm-bitcode)

;; Github README.mds.
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; company-mode.
(require 'company)
(require 'company-dabbrev)
(require 'helm-company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-ghc)
(setq company-idle-delay 0
      company-minimum-prefix-length 2
      company-show-numbers t
      company-selection-wrap-around t
      company-dabbrev-ignore-case t
      company-dabbrev-ignore-invisible t
      company-dabbrev-downcase nil
      company-backends (list #'company-css
                             #'company-clang
                             #'company-capf
                             (list #'company-dabbrev-code
                                   #'company-keywords)
                             #'company-files
                             #'company-dabbrev))
(global-company-mode t)
(define-key company-active-map (kbd "C-n")
  (lambda () (interactive) (company-complete-common-or-cycle 1)))
(define-key company-active-map (kbd "C-p")
  (lambda () (interactive) (company-complete-common-or-cycle -1)))

;; skewer-mode.
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; tagedit
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; alchemist
(require 'alchemist)
(setq alchemist-project-compile-when-needed t)

(provide 'setup-languages)
;;; setup-languages.el ends here
