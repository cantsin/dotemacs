;;; setup-misc -- Summary
;;; Commentary:
;;; Override global key bindings.
;;; Code:

;; easy access!
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; this should be already part of emacs.
(defun copy-line (&optional arg)
  "Copy the line, passing on ARG."
  (interactive "P")
  (save-excursion
    (read-only-mode t)
    (kill-line arg)
    (read-only-mode nil)))
(setq-default kill-read-only-ok t) ;; required for copy-line.
(global-set-key (kbd "C-c C-k") 'copy-line)

;; improve control-a
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; better narrow indirect support
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current \\
region (delimited by START and END), indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))
(global-set-key (kbd "C-x n x") 'narrow-to-region-indirect)

;; a proper delete-horizontal-space that ignores newlines.
(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))
(global-set-key (kbd "M-\\") 'kill-whitespace)

;; indent region, defun, or buffer
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun indent-region-or-defun ()
  "Indent a region if selected, otherwise the defun."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-defun)
        (message "Indented defun.")))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-defun)

;; occur-mode customizations
(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their 'major-mode` is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun occur-multi-occur ()
  "Start `multi-occur' for the current search term on all buffers \\
with the first matching buffer's major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode
    (with-current-buffer (car (nth 2 occur-revert-arguments))
      major-mode))
   (car occur-revert-arguments)))
(define-key occur-mode-map "m" 'occur-multi-occur)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; with prefix, kill current line from beginning of line
(defmacro bol-with-prefix (function)
  "Call FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument.  The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "endless/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))

;; toggle refill-mode on/off
(global-set-key (kbd "C-c q") 'refill-mode)

;; conveniently join lines.
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; pretty-print evals.
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(global-set-key [remap eval-expression] 'pp-eval-expression)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; windmove

(provide 'setup-keys)
;;; setup-keys.el ends here
