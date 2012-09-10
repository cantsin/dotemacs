(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-flex-matching t)
(setq ido-enable-prefix nil)
(setq ido-auto-merge-work-directories-length nil)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-virtual-buffers t)
(setq ido-handle-duplicate-virtual-buffers 2)
(setq ido-max-prospects 10)

(defun ido-for-mode (prompt the-mode)
  (switch-to-buffer
   (ido-completing-read prompt
                        (save-excursion
                          (delq
                           nil
                           (mapcar (lambda (buf)
                                     (when (buffer-live-p buf)
                                       (with-current-buffer buf
                                         (and (eq major-mode the-mode)
                                              (buffer-name buf)))))
                                   (buffer-list)))))))

;; todo: change to use ansi-term or eshell
;; todo: add one for erc-mode or use
;; (ido-buffer-internal ido-default-buffer-method nil nil nil "#")
(defun ido-shell-buffer()
  (interactive)
  (ido-for-mode "Shell: " 'shell-mode))

(global-set-key (kbd "C-c s") 'ido-shell-buffer)

(provide 'setup-ido)
