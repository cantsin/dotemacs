;; set up various languages here.

;; julia
(require 'julia-mode)

(setq auto-mode-alist
      (append '(("\\.jl$" . julia-mode)) auto-mode-alist))

;; python
(require 'jedi)
(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'auto-complete-mode)

(setq jedi:server-args
      '("--sys-path" "/usr/local/lib/python2.7/dist-packages"))

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

(provide 'setup-languages)
