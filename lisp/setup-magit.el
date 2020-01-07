;;; setup-magit -- Summary
;;; Commentary:
;;; Setup magit.
;;; Code:
(require 'use-package)

(use-package ediff
  :defer t
  :config (setq diff-switches "-u"
                ediff-diff-options "-w"
                ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-window-setup-function 'ediff-setup-windows-plain))

(defun magit-always-goto-previous-buffer ()
  "Fix: magit sometimes does not return to the previous buffer correctly."
  (setq previous-buffer-under-magit nil)
  (defadvice magit-mode-display-buffer (before cache-buffer-behind-magit activate)
    "Set previous buffer."
    (when (not (string-prefix-p "*magit" (buffer-name)))
      (setq previous-buffer-under-magit (current-buffer))))
  (defadvice magit-mode-quit-window (after restore-buffer-behind-magit activate)
    "Switch to previous buffer."
    (when previous-buffer-under-magit
      (switch-to-buffer previous-buffer-under-magit)
      (setq previous-buffer-under-magit nil))))

(use-package forge
  :after magit)

(use-package magit
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c f" . magit-fetch-all))
  :config
  (setq magit-diff-refine-hunk t
        magit-process-popup-time 10)
  (magit-always-goto-previous-buffer)
  ;; magit settings
  (set-face-foreground 'diff-context "#666666")
  (set-face-foreground 'diff-removed "#ff0000")
  (set-face-foreground 'diff-added "#00cc33")
  ;; custom gitea forge
  (add-to-list 'forge-alist '("git.home" "git.home/api/v1" "git.home" forge-gitea-repository))
)

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(provide 'setup-magit)
;;; setup-magit.el ends here
