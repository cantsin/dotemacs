;;; setup-misc -- Summary
;;; Commentary:
;;; Setup various bits that don't really go anywhere else.
;;; Code:

(require 'eldoc)

;; default hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(global-set-key (kbd "C-c s") 'flyspell-correct-word-before-point)

;; elfeed.
(global-set-key (kbd "C-x w") 'elfeed)

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

;; enable w3m
;;(require 'w3m-load)
;;(setq w3m-use-tab t)

(defun fix-coding-system ()
  "Fix annoying DOS or Mac line endings."
  (interactive)
  (progn
   (set-buffer-file-coding-system 'utf-8-unix)
   (save-buffer)))

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
                "png" "gif" "bmp" "tif" "jpeg" "jpg"))
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

;; webjump
(global-set-key (kbd "C-x g") 'webjump)

;; Add Urban Dictionary to webjump
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites
                '("Urban Dictionary" .
                  [simple-query
                   "www.urbandictionary.com"
                   "http://www.urbandictionary.com/define.php?term="
                   ""])))

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

;; by default, open as root if necessary
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (or (and buffer-file-name
                   (file-writable-p buffer-file-name))
              (equal major-mode 'dired-mode))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; hl-line
;; (require 'hl-line+)
;; (global-hl-line-mode +1)
;; (global-hl-line-highlight)

;; visual-regexp.
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

(require 'ace-jump-mode)

;; key chords. should be used sparingly.
(require 'key-chord)
(key-chord-define-global ",," 'ido-switch-buffer)
(key-chord-define-global ",." '(lambda ()
                                 (interactive)
                                 (switch-to-buffer (other-buffer (current-buffer) 1))))
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-mode +1)

(require 'powerline)
(powerline-default-theme)

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
