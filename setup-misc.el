
;; this should be already part of emacs.
(defun copy-line (&optional arg)
  (interactive "P")
  (save-excursion
    (toggle-read-only 1)
    (kill-line arg)
    (toggle-read-only 0)))

(setq-default kill-read-only-ok t) ;; required for copy-line.
(global-set-key "\C-c\C-k" 'copy-line)

;; save our point position between sessions elsewhere.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "save-place"))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; scroll compilation buffer by default
(setq compilation-scroll-output t)

;; default hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; paredit.
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
;; make paredit and eldoc play nice.
(eldoc-add-command 'paredit-backward-delete 'paredit-close-round)

;; smart-tab.
(require 'smart-tab)
(add-hook 'emacs-lisp-mode-hook 'smart-tab-mode)

;; uniquify buffers.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; diff.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq diff-switches "-u")

;; winner-mode.
(winner-mode 1)

;; paren-mode.
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

;; expand-mode.
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; toggle refill-mode on/off
(global-set-key (kbd "C-c q") 'refill-mode)

;; make footnotes easier to use
(global-set-key (kbd "C-c f") 'org-footnote-action)

;; enable some disabled commands.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; pretty-print evals.
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(global-set-key [remap eval-expression] 'pp-eval-expression)

;; disable overwrite-mode because it is really annoying.
(define-key global-map [(insert)] nil)

;; enable w3m
;;(require 'w3m-load)
;;(setq w3m-use-tab t)

;; fix annoying DOS or Mac line endings
(defun fix-coding-system ()
  (interactive)
  (progn
   (set-buffer-file-coding-system 'utf-8-unix)
   (save-buffer)))

;; hl-line
(require 'hl-line+)
(global-hl-line-highlight)

;; openwith
(when (require 'openwith nil 'noerror)
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
  (openwith-mode 1))

;; malyon -- z interpreter.
;; (require 'malyon)

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
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun occur-multi-occur ()
  "Starts multi-occur for the current search term on all buffers
with the first matching buffer's major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode
    (with-current-buffer (car (nth 2 occur-revert-arguments))
      major-mode))
   (car occur-revert-arguments)))
(define-key occur-mode-map "m" 'occur-multi-occur)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(provide 'setup-misc)
