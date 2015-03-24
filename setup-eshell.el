;;; setup-eshell -- Summary
;;; Commentary:
;;; Setup eshell.
;;; Code:
(require 'eshell)
(require 'em-smart)
(require 'em-prompt)
(require 'em-dirs)

(require 'tramp) ;; lets "sudo" work for some reason

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(defun eshell/emacs (&rest args)
  "Open a file in Emacs.  Some habits die hard.  Pass on ARGS."
  (if (null args)
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun eshell/clear ()
  "Scroll contents of eshell window out of sight, leaving a blank window."
  (interactive)
  (let ((number-newlines (count-lines (window-start) (point))))
    (insert (make-string number-newlines ?\n)))
    (eshell-send-input))

(defun eshell/dired ()
  "Call dired on the current directory."
  (dired (eshell/pwd)))

(defun eshell/sb (&rest args)
  "Switch to given buffer, apply ARGS."
  (funcall 'switch-to-buffer (apply 'eshell-flatten-and-stringify args)))

(defun eshell-view-file (file)
  "A version of `view-file' for FILE which properly respects the eshell prompt."
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
  "Invoke `view-file' on ARGS.  \"less +42 foo\" will go to \\
line 42 in the buffer for foo.."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (eshell-view-file file)
          (goto-char (point-min))
          (forward-line (1- line)))
      (eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

(defun invoke-bash (command)
  "For these situations where eshell won't work, simply invoke a bash COMMAND."
  (let ((invoke-bash-cmd (concat "bash -c \"" command "\"")))
    (message invoke-bash-cmd)
    (throw 'eshell-replace-command (eshell-parse-command invoke-bash-cmd))))


(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

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

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file.  The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  "Quick way to exit."
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(load "em-hist")           ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]") 'face `(:foreground "green"))
      )))

;; TODO: no autocomplete anymore
;; (setq eshell-prompt-function
;;       (lambda ()
;;         (concat
;;          (propertize ((lambda (p-lst)
;;             (if (> (length p-lst) 3)
;;                 (concat
;;                  (mapconcat (lambda (elm) (if (zerop (length elm)) ""
;;                                             (substring elm 0 1)))
;;                             (butlast p-lst 3)
;;                             "/")
;;                  "/"
;;                  (mapconcat (lambda (elm) elm)
;;                             (last p-lst 3)
;;                             "/"))
;;               (mapconcat (lambda (elm) elm)
;;                          p-lst
;;                          "/")))
;;           (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
;;          (or (curr-dir-git-branch-string (eshell/pwd)))
;;          (propertize "$ " 'face 'default))))

;; (setq eshell-highlight-prompt nil)

(provide 'setup-eshell)
;;; setup-eshell.el ends here
