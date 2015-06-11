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

(require 'eldoc)

;; default hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

;(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
;(global-set-key [remap kill-line] (bol-with-prefix paredit-kill))

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

;; purpose
(require 'window-purpose)
(purpose-mode)

(require 'typo)
(typo-global-mode 1)
(add-hook 'text-mode-hook 'typo-mode)

(require 'ocodo-svg-modelines)
(ocodo-svg-modelines-init)
(smt/set-theme 'ocodo-mesh-retro-aqua-smt)

(require 'ocodo-mesh-retro-aqua-smt)
(defun ocodo-mesh-retro-aqua-smt-background (theme)
  (let ((ocodo-twisted-stops '(("0%" "#FFFFFF" "0.1")
                               ("30%" "#FFFFFF" "0.1")
                               ("70%" "#000000" "0.1")
                               ("100%" "#000000" "0.1"))))
    (ocodo-smt-edge-image theme ocodo-mesh-retro-aqua-graphic)))

(defun ocodo-mesh-retro-aqua-smt-overlay (theme)
  (let ((ocodo-overlay-stops '(("0%" "#000000" "0.0")
                               ("0%" "#000000" "0.0")
                               ("80%" "#FFFFFF" "0.1")
                               ("100%" "#000000" "0.3"))))
    (ocodo-smt-overlay theme)))

(defun ocodo-mesh-retro-aqua-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "12pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-mesh-retro-aqua-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "12pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun ocodo-mesh-retro-aqua-info-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun ocodo-mesh-retro-aqua-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "12pt"
        :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun ocodo-mesh-retro-aqua-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun ocodo-mesh-retro-aqua-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-mesh-retro-aqua-version-control-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#60ACB1" "#365E63")))

(defun my/name-of-buffers (n)
  "Return the names of the first N buffers from `buffer-list'."
  (let ((bns
         (delq nil
               (mapcar
                (lambda (b)
                  (unless (string-match "^ " (setq b (buffer-name b)))
                    b))
                (buffer-list)))))
    (subseq bns 1 (min (1+ n) (length bns)))))

;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".
(defun my/number-names (list)
  "Enumerate and concatenate LIST."
  (let ((i 0))
    (mapconcat
     (lambda (x)
       (format "%d. %s" (cl-incf i) x))
     list
     ", ")))

(defvar my/last-buffers nil)

(defun my/switch-to-buffer (arg)
  (interactive "p")
  (switch-to-buffer
   (nth (1- arg) my/last-buffers)))

(defun my/switch-to-buffer-other-window (arg)
  (interactive "p")
  (switch-to-buffer-other-window
   (nth (1- arg) my/last-buffers)))

(global-set-key
 "\C-o"
 (defhydra my/switch-to-buffer (:exit t
                                :body-pre (setq my/last-buffers
                                                (my/name-of-buffers 5)))
   "
_o_ther buffers: %s(my/number-names my/last-buffers)

"
   ("o" my/switch-to-buffer "this window")
   ("O" my/switch-to-buffer-other-window "other window")
   ("<escape>" nil)))

(provide 'setup-misc)
;;; setup-misc.el ends here
