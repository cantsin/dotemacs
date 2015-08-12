;;; setup-languages -- Summary
;;; Commentary:
;;; Setup for various languages.
;;; Code:
(require 'use-package)

(use-package lua-mode
  :defer t
  :ensure t
  :config (setq lua-indent-level 2))

(use-package js2-mode
  :defer t
  :ensure t
  :config (progn
            (setq js-indent-level 2)
            (add-hook 'js-mode-hook 'js2-minor-mode)
            (add-hook 'js2-mode-hook 'ac-js2-mode)))

(use-package alchemist
  :defer t
  :ensure t
  :config (setq alchemist-project-compile-when-needed t))

(defun cantsin/setup-jedi ()
  "Configure jedi (python)."
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package jedi
  :defer t
  :ensure t
  :config (cantsin/setup-jedi))

;; Github README.mds.
(use-package markdown-mode
  :defer t
  :ensure t
  :config (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(use-package whitespace
  :ensure t
  :config (setq whitespace-style '(face trailing lines-tail tabs)
                whitespace-line-column 80
                global-whitespace-cleanup-mode t))

(use-package compile
  :defer t
  :ensure t
  :config (setq compilation-scroll-output t))

(use-package restclient
  :defer t
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode)))

;; auto disassemble llvm when opening .bc files
(use-package autodisass-llvm-bitcode
  :ensure t
  :defer t)

(defun load-agda ()
  "Load agda-mode on demand."
  (interactive)
  (condition-case nil
      (load-file (let ((coding-system-for-read 'utf-8))
                   (shell-command-to-string "agda-mode locate")))
    (error nil)))

;; built-ins.
(add-hook 'makefile-mode-hook 'indent-tabs-mode)
(setq c-basic-offset 4)
(c-set-offset 'substatement-open 0)

(provide 'setup-languages)
;;; setup-languages.el ends here
