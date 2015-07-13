;;; setup-magit -- Summary
;;; Commentary:
;;; Setup magit.
;;; Code:
(require 'use-package)

(defun cantsin/magit-init ()
  "Set up magit properly."
  (progn
    ;; we no longer need vc-git
    (delete 'Git vc-handled-backends)))

(defun magit-toggle-whitespace ()
  "Toggle whitespace."
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  "Ignore whitespace."
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  "Do not ignore whitespace."
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defun load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

(defun visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
     (replace-regexp-in-string
      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
      (magit-get "remote"
                 (magit-get-current-remote)
                 "url"))
     (magit-get-current-branch))))

(defun cantsin/magit-config ()
  "Configure magit appropriately."
  (progn
    (if (eq system-type 'windows-nt)
        (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe"))

    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    (use-package magit-gh-pulls
      :ensure t
      :config (progn
                (define-key magit-mode-map "#gg" 'load-gh-pulls-mode)
                (define-key magit-mode-map "V" 'visit-pull-request-url)))

    ;; magit settings
    (set-face-foreground 'diff-context "#666666")
    (set-face-foreground 'diff-added "#00cc33")
    (set-face-foreground 'diff-removed "#ff0000")

    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
          magit-status-buffer-switch-function 'switch-to-buffer
          magit-diff-refine-hunk t
          magit-rewrite-inclusive 'ask
          magit-process-popup-time 10
          magit-set-upstream-on-push 'askifnotset)))

(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
  :init (cantsin/magit-init)
  :config (cantsin/magit-config))

(provide 'setup-magit)
;;; setup-magit.el ends here
