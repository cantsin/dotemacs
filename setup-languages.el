;;; setup-languages -- Summary
;;; Commentary:
;;; Setup for various languages.
;;; Code:

;; clojure
(require 'nrepl)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-lein-command "~/bin/lein")

(add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

;; julia
(require 'julia-mode)

(setq auto-mode-alist
      (append '(("\\.jl$" . julia-mode)) auto-mode-alist))

;; python
;; TODO adjust for 3.x
;; (require 'jedi)
;; (setq jedi:setup-keys t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'auto-complete-mode)

;; (setq jedi:server-args
;;       '("--sys-path" "/usr/local/lib/python2.7/dist-packages"))

;; elisp
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c v") 'eval-buffer)
;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; (require 'pretty-mode)
;; (global-pretty-mode 1)

(require 'smart-compile)

;; work in progress. requires some setup. no error checking!
(defun recompile-if-extant ()
  "Recompile, assumes the *compilation* frame exists."
  (interactive)
  (switch-to-buffer-other-frame "*compilation*")
  (recompile))

(global-set-key (kbd "C-c C-c") 'recompile-if-extant)

;; bury compilation when successful
;; (add-to-list 'compilation-finish-functions
;;              (lambda (buffer msg)
;;                (when (bury-buffer buffer)
;;                  (replace-buffer-in-windows buffer))))

(electric-indent-mode +1)
(add-hook 'python-mode-hook
          #'(lambda () (setq electric-indent-mode nil)))

;; always use the same window.
(setq compilation-buffer-name-function
      '(lambda (mode)
     "*compilation*"))

(setq compilation-scroll-output 'first-error)

;; use M-. and M-, to jump to definitions
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook
          (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode))

;; haskell
(add-to-list 'smart-compile-alist
             '("\\.hs\\'" . "ghc -o %n --make %f"))
(add-to-list 'smart-compile-alist
             '("\\.rs\\'" . "rust build %f"))

;; litable, very experimental
(require 'litable)

;; flycheck ftw
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(provide 'setup-languages)
;;; setup-languages.el ends here
