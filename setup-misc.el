;;; setup-misc -- Summary
;;; Commentary:
;;; Setup various bits that don't really go anywhere else.
;;; Code:

(require 'eldoc)

;; default hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(require 'flyspell)
(global-set-key (kbd "C-c s") 'flyspell-correct-word-before-point)
(define-key flyspell-mode-map (kbd "M-g n") 'flyspell-goto-next-error)

(defun flyspell-goto-previous-error (&optional arg)
  "Count ARG mis-spelled words backwards."
  (interactive)
  (let ((pos1 (point))
	(pos  (point))
	(arg  (if (or (not (numberp arg)) (< arg 1)) 1 arg))
	ov ovs)
    (if (catch 'exit
	  (while (and (setq pos (previous-overlay-change pos))
		      (not (= pos pos1)))
	    (setq pos1 pos)
	    (if (> pos (point-min))
		(progn
		  (setq ovs (overlays-at (1- pos)))
		  (while (consp ovs)
		    (setq ov (car ovs))
		    (setq ovs (cdr ovs))
		    (if (and (flyspell-overlay-p ov)
			     (= 0 (setq arg (1- arg))))
			(throw 'exit t)))))))
        (progn
         (setq flyspell-old-pos-error pos)
         (setq flyspell-old-buffer-error (current-buffer))
         (goto-char pos)
         (backward-word))
      (error "No word to correct before point"))))
(define-key flyspell-mode-map (kbd "M-g p") 'flyspell-goto-previous-error)

;; smart-tab.
(require 'smart-tab)
(add-hook 'emacs-lisp-mode-hook 'smart-tab-mode)

;; uniquify buffers.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; diff.
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq diff-switches "-u")

;; winner-mode.
(winner-mode 1)

;; expand-mode.
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; toggle refill-mode on/off
(global-set-key (kbd "C-c q") 'refill-mode)

;; make footnotes easier to use
(global-set-key (kbd "C-c f") 'org-footnote-action)

;; make {back,for}ward-paragraph easier to use
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; enable some disabled commands.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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

;; disable overwrite-mode because it is really annoying.
(define-key global-map [(insert)] nil)

;; openwith
(require 'openwith)
(setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
              '("mp4" "avi" "mov" "flv"
                "ogm" "ogg" "mkv"))
             "mplayer"
             '(file))
       (list (openwith-make-extension-regexp
              '("xbm" "pbm" "pgm" "ppm" "pnm"
                "png" "bmp" "tif")) ;; gif/jpg not included; we want to inline these
             "feh"
             '(file))
       (list (openwith-make-extension-regexp
              '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
             "libreoffice"
             '(file))
       (list (openwith-make-extension-regexp
              '("pdf" "ps" "ps.gz" "dvi"))
             "evince"
             '(file))))
(openwith-mode 1)

(defun visit-eshell-buffer ()
  "Create or visit an eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))

(global-set-key (kbd "C-c t") 'visit-eshell-buffer)

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

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

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

;; more whitespace stuff.
(setq global-whitespace-cleanup-mode t)

;; hugely useful
(global-set-key (kbd "C-x p") 'proced)

;; visual-regexp.
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

(require 'ace-jump-mode)

;; key chords. should be used sparingly.
(require 'key-chord)
(key-chord-define-global ",," 'helm-mini)
(key-chord-define-global ",." '(lambda ()
                                 (interactive)
                                 (switch-to-buffer (other-buffer (current-buffer) 1))))
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-mode +1)

(defun url-decode-region (start end)
  "URL decode a region from START to END."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(require 'wgrep)
(setq wgrep-enable-key "r")
(setq wgrep-auto-save-buffer t)

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

(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix paredit-kill))

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; use abbrev to memorize mistakes
(global-set-key (kbd "C-x C-i") 'endless/ispell-word-then-abbrev)

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

(setq save-abbrevs t)
(setq-default abbrev-mode t)

;; write good (sic).
(require 'writegood-mode)
(global-set-key "\C-cw" 'writegood-mode)
(global-set-key "\C-c\C-gg" 'writegood-grade-level)
(global-set-key "\C-c\C-ge" 'writegood-reading-ease)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
;; Restclient
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;; turn off annoying ffap behavior
(require 'ffap)
(setq ffap-alist nil
      ffap-machine-p-known 'accept
      ffap-require-prefix nil
      ffap-gopher-regexp nil
      ffap-url-regexp nil
      ffap-ftp-regexp nil
      ffap-ftp-sans-slash-regexp nil
      ffap-rfs-regexp nil
      ffap-shell-prompt-regexp nil)
(defun ffap-file-at-point nil
  "Turn off ffap file-at-point completely."
  nil)

(setq ispell-personal-dictionary (concat user-emacs-directory "personal-dict"))

(require 'tiny)
(tiny-setup-default)

(provide 'setup-misc)
;;; setup-misc.el ends here
