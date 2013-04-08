;; set up various languages here.
(require 'julia-mode)

(setq auto-mode-alist
      (append '(("\\.jl$" . julia-mode)) auto-mode-alist))

;; (require 'pretty-mode)
;; (global-pretty-mode 1)

;; python mode
(elpy-enable)

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
