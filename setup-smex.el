;; set smex up so that it is loaded when needed.
(global-set-key (kbd "M-x") (lambda ()
                              (interactive)
                              (or (boundp 'smex-cache)
                                  (smex-initialize))
                              (global-set-key [(meta x)] 'smex)
                              (smex)))

(global-set-key (kbd "C-c M-x") (lambda ()
                                  (interactive)
                                  (or (boundp 'smex-cache)
                                      (smex-initialize))
                                  (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                  (smex-major-mode-commands)))

;; C-h f: run describe-function on the currently selected command.
;; M-.: jump to the definition of the selected command.

(provide 'setup-smex)
