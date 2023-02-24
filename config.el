;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; turn off menu, tool, scrollbar as soon as possible
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "James Tranovich"
      user-mail-address "james@tranovich.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font "Triplicate T3c-16"
      doom-variable-pitch-font "Valkyrie T4-16")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/todos/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(global-subword-mode t)

(setq confirm-kill-emacs nil
      inhibit-compacting-font-caches t
      line-number-mode nil
      column-number-mode nil
      size-indication-mode t)

(setq doom-vibrant-brighter-comments t)
(setq frame-title-format "%b : emacs")

(setq doom-modeline-buffer-file-name-style 'relative-to-project
      doom-modeline-buffer-encoding nil
      doom-modeline-percent-position nil)

(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

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

(map! "C-?" #'help-command
      "C-h" #'delete-backward-char
      "M-h" #'backward-kill-word
      "M-[" #'backward-paragraph
      "M-]" #'forward-paragraph
      "M-\\" #'kill-whitespace ;; custom
      "C-c g" #'magit-status
      "M-g s" #'counsel-projectile-rg
      "M-g l" #'flycheck-list-errors
      "C-x C-j" #'dired-jump
      "M-g M-g" #'avy-goto-line
      "C->" #'mc/mark-next-like-this
      "C-<" #'mc/mark-previous-like-this
      "C-S-J" #'windmove-left
      "C-S-K" #'windmove-down
      "C-S-L" #'windmove-up
      "C-:" #'windmove-right)

(map! :map org-mode-map
      :after org
      "C-c a" #'org-agenda
      "C-c j" #'org-journal-new-entry
      "C-c r" #'org-cut-subtree)

(key-chord-mode +1)
(key-chord-define-global ",," #'ivy-switch-buffer)
(key-chord-define-global ",." '(lambda ()
                                 (interactive)
                                 (switch-to-buffer (caar (window-prev-buffers)))))
(key-chord-define-global "jj" #'avy-goto-char)

(require 'org)
(require 'request)

(defun log-todo-completion ()
  "Log todo completion."
  (interactive)
  (org-copy-subtree)
  (let* ((data `(("repeated" . ,(org-get-repeat))
                 ("state" . ,org-state)
                 ("todo" . ,org-subtree-clip))))
    (request "https://api.tranovich.io/activity"
      :type "POST"
      :data (json-encode data)
      :headers '(("Content-Type" . "application/json"))
      :parser 'buffer-string
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)))
      :complete (lambda (&rest _) (message "Written.")))))

(add-hook 'org-after-todo-state-change-hook 'log-todo-completion)

(setq org-journal-dir "~/journal"
      org-log-into-drawer nil
      org-log-repeat nil
      org-todo-keywords
      '((sequence "TODO(t)" "PENDING(p)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "FUTURE(l)")
        (sequence "|" "CANCELED(c)" "BLOCKED(x)")))

(use-package! lsp-mode
  :config
  (lsp-register-custom-settings
   '(("gopls.experimentalWorkspaceModule" t t)
     ("gopls.staticcheck" t t))))

(add-hook 'web-mode-hook #'rainbow-mode)

(use-package! stripe-buffer
  :init
  (add-hook 'org-mode-hook 'turn-on-stripe-table-mode))

(custom-set-faces!
 '(outline-1 :weight normal)
 '(outline-2 :weight normal)
 '(outline-3 :weight normal)
 '(outline-4 :weight normal)
 '(outline-5 :weight normal)
 '(outline-6 :weight normal)
 '(org-priority :weight bold)
 '(org-block :background "#2a2e38")
 '(stripe-highlight :background "#2a2e38"))

(after! org
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t))

(add-function
 :after after-focus-change-function
 (lambda () (save-some-buffers t)))

(defadvice switch-to-buffer (before save-buffer-now activate)
  "Save when switching to buffer."
  (when buffer-file-name (save-buffer)))

(setq projectile-files-cache-expire 5)

(defun set-flutter-sdk-dir ()
  (setq lsp-dart-flutter-sdk-dir
        (string-trim-right (shell-command-to-string "echo $FLUTTER_SDK"))))

(use-package dart-mode
  :init
  (add-hook 'dart-mode-hook 'set-flutter-sdk-dir))

(defun zig-documentation ()
  (interactive)
  (eww-open-file
   (expand-file-name "../../doc/langref.html" (executable-find "zig"))))

(defun zig-build-run ()
  "Run using `zig build run`."
  (interactive)
  (zig--run-cmd "build run"))

(use-package! zig-mode
  :mode "\\.zig\\'"
  :bind (("C-c z" . zig-build-run)
         ("C-c d" . zig-documentation)))
