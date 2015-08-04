;;; setup-misc -- Summary
;;; Commentary:
;;; Override key bindings.
;;; Code:
(require 'use-package)

(defun cantsin/key-init ()
  (key-chord-define-global ",," 'helm-mini)
  (key-chord-define-global ",." '(lambda ()
                                   (interactive)
                                   (switch-to-buffer (other-buffer (current-buffer) 1))))
  (key-chord-mode +1))

(use-package key-chord
  :defer t
  :init (cantsin/key-init))

(defun cantsin/mc-init ()
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package multiple-cursors
  :defer t
  :init (cantsin/mc-init))

(defun cantsin/writegood-init ()
  (global-set-key "\C-cw" 'writegood-mode)
  (global-set-key "\C-c\C-gg" 'writegood-grade-level)
  (global-set-key "\C-c\C-ge" 'writegood-reading-ease))

(use-package writegood-mode
  :defer t
  :init (cantsin/writegood-init))

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
;(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
;(global-set-key [remap kill-line] (bol-with-prefix paredit-kill))

;; use abbrev to memorize mistakes
(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'.  Then create an abbrev for the correction made.
With prefix P, create local abbrev.  Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))
(global-set-key (kbd "C-x C-i") 'endless/ispell-word-then-abbrev)

;; toggle refill-mode on/off
(global-set-key (kbd "C-c q") 'refill-mode)

;; make footnotes easier to use
(global-set-key (kbd "C-c f") 'org-footnote-action)

;; make {back,for}ward-paragraph easier to use
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; disable overwrite-mode because it is really annoying.
(define-key global-map [(insert)] nil)

;; conveniently join lines.
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; avy.
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)

;; quick search.
(global-set-key (kbd "M-g s") 'helm-ag)

(provide 'setup-keys)
;;; setup-keys.el ends here
