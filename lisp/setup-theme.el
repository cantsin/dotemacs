;;; setup-theme -- Summary
;;; Commentary:
;;; Setup theme modifications.
;;; Code:
(require 'use-package)

;; if icons do not show up:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package flycheck)

(defun -custom-modeline-github-vc ()
  "Set up git icon."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:foreground "darkorange" :background "gray17" :family ,(all-the-icons-octicon-family) :height 1.2)
                 'display '(raise -0.1))
     (propertize (format " %s" branch)
                 'face `(:foreground "darkorange" :background "gray17")))))

(defun custom-modeline-vc ()
  "Set up VC if applicable."
  (when vc-mode
    (-custom-modeline-github-vc)))

;; TODO issue-opened octicon for flycheck issues
(defun custom-modeline-flycheck-status ()
  "Set up flycheck status."
  (let* ((text (pcase flycheck-last-status-change
                (`finished (if flycheck-current-errors
                               (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                              (+ (or .warning 0) (or .error 0)))))
                                 (format "✖ %s Issue%s" count (if (eq 1 count) "" "s")))
                             "✔"))
                (`running     "⟲")
                (`no-checker  "⚠")
                (`not-checked "✖")
                (`errored     "⚠")
                (`interrupted "⛔")
                (`suspicious  ""))))
     (propertize text
                 'help-echo "Show Flycheck Errors"
                 'mouse-face '(:box 1)
                 'face `(:foreground "gray90" :background "gray40")
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(defun custom-modeline-file-and-icon ()
  "Customized modeline file."
  (format "%s%s"
          (propertize (all-the-icons-icon-for-buffer)
                      'help-echo (format "Major-mode: `%s`" major-mode)
                      'face `(:foreground "gray90" :background "gray40" :family ,(all-the-icons-icon-family-for-buffer))
                      'display '(raise -0.1))
          (propertize (concat " "
                              (file-relative-name buffer-file-name (projectile-project-root)))
                      'face `(:foreground "gray90" :background "gray40"))))

(defun custom-modeline-project ()
  "Customized modeline project."
  (format "%s"
          (propertize (projectile-project-name)
                      'face `(:foreground "gray80" :background "gray17"))))

(defun custom-modeline-location ()
  "Customized modeline location."
  (format "%s"
          (propertize (format-mode-line " %l:%c")
                      'face `(:foreground "black" :background "darkorange"))))

;; TODO startup issue

(defun powerline-arrow (before after where)
  "BEFORE color. AFTER color. WHERE direction. Powerline arrow."
  (format "%s%s%s"
          (propertize " " 'face `(:background ,before))
          (propertize " " 'display
                       (funcall
                        (intern
                         (format "powerline-%s-%s"
                                 (powerline-current-separator)
                                 (symbol-name where)))
                        'powerline-active2 'powerline-active1)
                       'face `(:background ,after :foreground ,before)
                       )
          (propertize " " 'face `(:background ,after))))

(custom-set-faces
 '(mode-line ((t (:background "gray40"))))
 '(mode-line-inactive ((t (:background "gray40")))))

(setq-default mode-line-format
              '("%e"
                (:eval (custom-modeline-location))
                (:eval (powerline-arrow "darkorange" "gray17" 'left))

                (:eval (custom-modeline-project))
                (:eval (powerline-arrow "gray17" "gray40" 'left))

                (:eval (custom-modeline-file-and-icon))
                (:eval (powerline-arrow "gray40" "gray17" 'left))

                (:eval (custom-modeline-vc))
                (:eval (powerline-arrow "gray17" "gray40" 'right))

                (:eval (custom-modeline-flycheck-status))))

(use-package moe-theme
  :ensure t
  :config (progn
            (moe-dark)
            (moe-theme-set-color 'orange)))

(provide 'setup-theme)
;;; setup-theme.el ends here
