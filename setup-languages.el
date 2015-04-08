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
