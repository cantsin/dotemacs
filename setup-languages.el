;; set up various languages here.

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

;; use mode-compile after saving; bind to keypress.
(global-set-key '[(ctrl c) (ctrl %)]
                (lambda ()
                  (interactive)
                  (if (member 'smart-compile after-save-hook)
                      (progn
                        (setq after-save-hook
                              (remove 'smart-compile after-save-hook))
                        (message "No longer compiling after saving."))
                    (progn
                      (add-to-list 'after-save-hook 'smart-compile)
                      (message "Compiling after saving.")))))

;; bury compilation when successful
(add-to-list 'compilation-finish-functions
             (lambda (buffer msg)
               (when (bury-buffer buffer)
                 (replace-buffer-in-windows buffer))))

(setq compilation-buffer-name-function
      (lambda (mode) (concat "*" (downcase mode) ": " (buffer-name) "")))

(setq compilation-scroll-output 'first-error)

;; haskell
(add-to-list 'smart-compile-alist
             '("\\.hs\\'" . "ghc -o %n --make %f"))
(add-to-list 'smart-compile-alist
             '("\\.rs\\'" . "rust build %f"))

(provide 'setup-languages)
