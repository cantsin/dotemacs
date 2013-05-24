(require 'eshell)
(require 'em-smart)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/dired ()
  (dired (eshell/pwd)))

(defun eshell/sb (&rest args)
  "Switch to given buffer."
  (funcall 'switch-to-buffer (apply 'eshell-flatten-and-stringify args)))

(defun eshell-view-file (file)
  "A version of `view-file' which properly respects the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
        (buffer (find-file-noselect file)))
    (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
                               (+ (window-point)
                                  (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on a file. \"less +42 foo\" will go to line 42 in
the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (eshell-view-file file)
          (goto-line line))
      (eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

(defun invoke-bash (command)
  (let ((invoke-bash-cmd (concat "bash -c \"" command "\"")))
    (message invoke-bash-cmd)
    (throw 'eshell-replace-command (eshell-parse-command invoke-bash-cmd))))

(defun eshell/git (&rest command)
  (if (null command)
      (call-interactively #'magit-status)
    (cond
     ((equal (first command) "status")
      (call-interactively #'magit-status))
     ((equal (first command) "diff")
      (call-interactively #'magit-diff-working-tree))
     (t
      ;; does not quite work yet
      (invoke-bash (concat "git " (mapconcat 'identity command " ")))))))

(defun eshell/info (subject)
  "Read the Info manual on SUBJECT."
  (let ((buf (current-buffer)))
    (Info-directory)
    (let ((node-exists (ignore-errors (Info-menu subject))))
      (if node-exists
          0
        ;; We want to switch back to *eshell* if the requested
        ;; Info manual doesn't exist.
        (switch-to-buffer buf)
        (eshell-print (format "There is no Info manual on %s.\n" subject))
        1))))

(provide 'setup-eshell)