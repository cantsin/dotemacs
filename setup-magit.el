;;; setup-magit -- Summary
;;; Commentary:
;;; Setup magit.
;;; Code:
(require 'magit)

(if (eq system-type 'windows-nt)
    (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe"))

(set-face-foreground 'diff-context "#666666")
(set-face-foreground 'diff-added "#00cc33")
(set-face-foreground 'diff-removed "#ff0000")

(global-set-key (kbd "C-c g") 'magit-status)

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

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  "Fullscreen magit."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration, kill magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(require 'magit-gh-pulls)

;; don't immediately query for github PRs unless we explicitly try.
(eval-after-load 'magit
  '(define-key magit-mode-map "#gg"
     'endless/load-gh-pulls-mode))

(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

(defun endless/visit-pull-request-url ()
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

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'endless/visit-pull-request-url))

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

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  "Fullscreen magit."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration, kill magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(require 'magit-gh-pulls)

;; don't immediately query for github PRs unless we explicitly try.
(eval-after-load 'magit
  '(define-key magit-mode-map "#gg"
     'endless/load-gh-pulls-mode))

(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

;; fix annoying problem with git commit mode
(add-hook 'git-commit-mode-hook
          '(lambda () (beginning-of-buffer))
          t)

(provide 'setup-magit)
;;; setup-magit.el ends here
