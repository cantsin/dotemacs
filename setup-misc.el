;;; setup-misc -- Summary
;;; Commentary:
;;; Setup various bits that don't really go anywhere else.
;;; Code:
(require 'use-package)

(use-package ediff
  :defer t
  :config (setq diff-switches "-u"
                ediff-diff-options "-w"
                ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package eldoc
  :defer t)

(use-package wgrep
  :defer t
  :config (setq wgrep-enable-key "r"
                wgrep-auto-save-buffer t))

;; turn off annoying ffap behavior
(use-package ffap
  :defer t
  :config (setq ffap-alist nil
                ffap-machine-p-known 'accept
                ffap-require-prefix nil
                ffap-gopher-regexp nil
                ffap-url-regexp nil
                ffap-ftp-regexp nil
                ffap-ftp-sans-slash-regexp nil
                ffap-rfs-regexp nil
                ffap-shell-prompt-regexp nil))
(defun ffap-file-at-point nil
  "Turn off ffap file-at-point completely."
  nil)

(use-package window-purpose
  :init (purpose-mode))

(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;; agda.
(condition-case nil
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))
  (error nil))

(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor will move \\
Also, if the last command was a copy - skip past all the expand-region cruft."
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(defun url-decode-region (start end)
  "URL decode a region from START to END."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(require 'epg)
(defun epg--check-error-for-decrypt (context)
  "CONTEXT is an epg context.
Here, we work around a bug in epg where gpg prints out \"Good
signature\" and is yet otherwise successful."
  (let ((errors (epg-context-result-for context 'error)))
    (if (epg-context-result-for context 'decryption-failed)
	(signal 'epg-error
		(list "Decryption failed" (epg-errors-to-string errors))))
    (unless (or (epg-context-result-for context 'decryption-okay)
                (equal errors '((exit))))
      (signal 'epg-error
	      (list "Can't decrypt" (epg-errors-to-string errors))))))

(provide 'setup-misc)
;;; setup-misc.el ends here
