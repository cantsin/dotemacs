;;; setup-magit -- Summary
;;; Commentary:
;;; Setup magit.
;;; Code:
(require 'use-package)

(defun cantsin/magit-init ()
  "Set up magit properly."
  (progn
    ;; we no longer need vc-git
    (delete 'Git vc-handled-backends)
    ;; make magit status go full-screen but remember previous window
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    ;; Close popup when commiting - this stops the commit window
    ;; hanging around
    ;; From: http://git.io/rPBE0Q
    (defadvice git-commit-commit (after delete-window activate)
      (delete-window))
    (defadvice git-commit-abort (after delete-window activate)
      (delete-window))
    ;; these two force a new line to be inserted into a commit window,
    ;; which stops the invalid style showing up.
    ;; From: http://git.io/rPBE0Q
    (defun magit-commit-mode-init ()
      (beginning-of-buffer)
      (when (looking-at "\n")
        (open-line 1)))

    (add-hook 'git-commit-mode-hook 'magit-commit-mode-init)))

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

(defun magit-quit-session ()
  "Restore the previous window configuration, kill magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

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
    ;; restore previously hidden windows
    (defadvice magit-quit-window (around magit-restore-screen activate)
      (let ((current-mode major-mode))
        ad-do-it
        ;; we only want to jump to register when the last seen buffer
        ;; was a magit-status buffer.
        (when (eq 'magit-status-mode current-mode)
          (jump-to-register :magit-fullscreen))))

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
