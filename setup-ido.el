;;; setup-ido -- Summary
;;; Commentary:
;;; Setup ido.
;;; Code:
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-enable-prefix nil)
(setq ido-auto-merge-work-directories-length nil)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-virtual-buffers t)
(setq ido-handle-duplicate-virtual-buffers 2)
(setq ido-max-prospects 10)

(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))

(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  "Fix ido-ubiquitous for newer packages."
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

(provide 'setup-ido)
;;; setup-ido.el ends here
