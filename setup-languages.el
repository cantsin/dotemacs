;;; setup-languages -- Summary
;;; Commentary:
;;; Setup for various languages.
;;; Code:

;; clojure
(require 'cider)
(setq cider-popup-stacktraces nil)

(add-to-list 'same-window-buffer-names "*cider*")

;; default hooks.
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'subword-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;;(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
;;(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; pretty-print evals.
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(global-set-key [remap eval-expression] 'pp-eval-expression)

;; paredit.
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

;; make paredit and eldoc play nice.
(eldoc-add-command 'paredit-backward-delete 'paredit-close-round)

;; paren-mode.
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

;; flymake.
(require 'flymake)
(setq flymake-gui-warnings-enabled nil)

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

(require 'smart-compile)

;; work in progress. requires some setup. no error checking!
(defun recompile-if-extant ()
  "Recompile, assumes the *compilation* frame exists."
  (interactive)
  (switch-to-buffer-other-frame "*compilation*")
  (recompile))

(global-set-key (kbd "C-c C-c") 'recompile-if-extant)

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

;; flycheck ftw
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; racer -- only comment out if installed
;; (setq racer-rust-src-path "~/rust/src/")
;; (setq racer-cmd "~/racer/target/release/racer")
;; (add-to-list 'load-path "~/racer/editors")
;; (eval-after-load "rust-mode" '(require 'racer))

;; indentation.
(require 'js)
(setq js-indent-level 2)
(setq c-basic-offset 4)
(c-set-offset 'substatement-open 0)
(setq lua-indent-level 2)

(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; elm-repl
(setenv "PATH" (concat (getenv "PATH") ":~/.cabal/bin"))
(setq exec-path (append exec-path '("~/.cabal/bin")))

(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-f") 'gofmt)))
(add-hook 'before-save-hook 'gofmt-before-save)
;; go get code.google.com/p/go.tools/cmd/godoc
(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-k") 'godoc)))
;; go get code.google.com/p/rog-go/exp/cmd/godef

;; auto disassemble llvm when opening .bc files
(require 'autodisass-llvm-bitcode)

;; Github README.mds.
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; company-mode.
(require 'company)
(require 'helm-company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-ghc)

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

;; validate open/closed braces in html.
(defun flymake-html-init ()
  "Initialize flymake for HTML."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "tidy" (list local-file))))

(defun flymake-html-load ()
  "Load flymake for HTML."
  (interactive)
  (when (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
    (set (make-local-variable 'flymake-allowed-file-name-masks)
         '(("\\.html\\|\\.ctp\\|\\.ftl\\|\\.jsp\\|\\.php\\|\\.erb\\|\\.rhtml" flymake-html-init))
         )
    (set (make-local-variable 'flymake-err-line-patterns)
         ;; pick up errors and warnings for HTML5
         '(("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(missing.*\\|discarding.*\\)" nil 1 2 4))
         )
    (flymake-mode t)))

(add-hook 'web-mode-hook 'flymake-html-load)
(add-hook 'html-mode-hook 'flymake-html-load)
(add-hook 'nxml-mode-hook 'flymake-html-load)
(add-hook 'php-mode-hook 'flymake-html-load)

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(flycheck-define-checker purs-check
  "Use purscheck to flycheck PureScript code."
  :command ("purscheck" source source-original temporary-file-name)
  :error-patterns
  ((error line-start
          (or (and "Error at " (file-name) " line " line ", column " column ":" (zero-or-more " "))
              (and "\"" (file-name) "\" (line " line ", column " column "):"))
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (zero-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (zero-or-more " ")
                                  (one-or-more not-newline)))))
          line-end))
  :modes purescript-mode)

;; connect flycheck with purscheck
(add-to-list 'flycheck-checkers 'purs-check)
(add-to-list 'load-path "purscheck.el") ;; custom
(setq flycheck-purs-check-executable "/home/james/.cabal/bin/purscheck")

;; enable purescript smart indentation and purscheck whenever a
;; purescript file is loaded into emacs
(require 'purescript-mode)
(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
(add-hook 'purescript-mode-hook 'flycheck-mode)

(provide 'setup-languages)
;;; setup-languages.el ends here
