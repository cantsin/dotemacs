;;; setup-theme -- Summary
;;; Commentary:
;;; Setup theme modifications.
;;; Code:
(require 'use-package)

;; if icons do not show up:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :demand t)

(use-package powerline
  :demand t)

(defun custom-modeline-vc ()
  "Set up VC if applicable."
  (cond
   (vc-mode
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:foreground "LightGoldenrod" :background "gray17" :family ,(all-the-icons-octicon-family) :height 1.2)
                   'display '(raise -0.1))
       (propertize (format " %s" branch)
                   'face `(:foreground "LightGoldenrod" :background "gray17")))))
   ((string-prefix-p "magit" (buffer-name))
    (propertize (format "%s" (all-the-icons-faicon "git"))
                'face `(:foreground "LightGoldenrod" :background "gray17" :family ,(all-the-icons-faicon-family) :height 1.2)
                'display '(raise -0.1)))
   (t
    (propertize (format "%s" (all-the-icons-faicon "minus-circle"))
                'face `(:foreground "LightGoldenrod" :background "gray17" :family ,(all-the-icons-faicon-family) :height 1.2)
                'display '(raise -0.1)))))

(defun custom-modeline-flycheck-status ()
  "Set up flycheck status."
  (let* ((count
          (if flycheck-current-errors
              (let ((n (let-alist (flycheck-count-errors flycheck-current-errors)
                         (+ (or .warning 0) (or .error 0)))))
                n)
            0))
         (string (if flycheck-current-errors
                     (format "%s error%s" count (if (eq 1 count) "" "s"))
                   ""))
         (icon (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors "bug" "check"))
                 (`running     "refresh")
                 (`no-checker  "exclamation-triangle")
                 (`not-checked "eye-slash")
                 (`errored     "bolt")
                 (`interrupted "exclamation-circle")
                 (`suspicious  "commenting-o"))))
    (format "%s %s"
            (propertize (format "%s" (all-the-icons-faicon icon))
                        'face `(:foreground "gray90" :background "gray40" :family ,(all-the-icons-faicon-family) :height 1.2)
                        'display '(raise -0.1))
            (propertize string
                        'help-echo "Show Flycheck Errors"
                        'mouse-face '(:box 1)
                        'face `(:foreground "gray90" :background "gray40")
                        'local-map (make-mode-line-mouse-map
                                    'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))))

(defun custom-modeline-read-only ()
  "Customized modeline file status."
  (if (equal (format-mode-line "%*") "%")
      (format "%s "
              (propertize (all-the-icons-octicon "lock")
                          'face `(:foreground "gray90" :family ,(all-the-icons-octicon-family) :height 1.2)
                          'display '(raise -0.1)))))

(defun custom-modeline-file-and-icon ()
  "Customized modeline file."
  (format "%s%s"
          (propertize (format "%s" (if (all-the-icons-icon-family-for-buffer)
                                       (all-the-icons-icon-for-buffer)
                                     (all-the-icons-faicon "question-circle-o")))
                      'help-echo (format "Major-mode: `%s`" major-mode)
                      'face `(:foreground "gray90" :background "gray40" :family ,(all-the-icons-icon-family-for-buffer))
                      'display '(raise -0.1))
          (propertize (concat " " (buffer-name))
                      'help-echo (buffer-file-name)
                      'face `(:foreground "gray90" :background "gray40"))))

(defun custom-modeline-project ()
  "Customized modeline project."
  (format "%s"
          (propertize (projectile-project-name)
                      'help-echo (projectile-project-root)
                      'face `(:foreground "gray80" :background "gray17"))))

(defun custom-modeline-location ()
  "Customized modeline location."
  (format "%s"
          (propertize (concat
                       " "
                       (format "%3d" (string-to-number (format-mode-line "%l")))
                       ":"
                       (format "%3d" (string-to-number (format-mode-line "%c"))))
                      'face `(:foreground "black" :background ,(if (powerline-selected-window-active) "LightGoldenrod" "LightSkyBlue") :weight bold))))

(setq-default frame-title-format
              '(:eval
                (format "emacs %s"
                        (cond
                         (buffer-file-truename
                          buffer-file-truename)
                         (dired-directory
                          (concat "[" dired-directory "]"))
                         (t
                          (concat "(" (string-trim (buffer-name)) ")"))))))

(defface inactive-face '((t :background "LightSkyBlue")) "Theme." :group 'setup-theme-faces)
(defface golden-face '((t :background "LightGoldenrod")) "Theme." :group 'setup-theme-faces)
(defface gray17-face '((t :background "gray17")) "Theme." :group 'setup-theme-faces)
(defface gray40-face '((t :background "gray40")) "Theme." :group 'setup-theme-faces)

(setq-default mode-line-format
              '("%e"
                (:eval (custom-modeline-location))
                (:eval (powerline-render (list
                                          (powerline-raw " " (if (powerline-selected-window-active) 'golden-face 'inactive-face))
                                          (powerline-arrow-left (if (powerline-selected-window-active) 'golden-face 'inactive-face) 'gray17-face)
                                          (powerline-raw " " 'gray17-face))))

                (:eval (custom-modeline-project))
                (:eval (powerline-render (list
                                          (powerline-raw " " 'gray17-face)
                                          (powerline-arrow-left 'gray17-face 'gray40-face)
                                          (powerline-raw " " 'gray40-face))))

                (:eval (custom-modeline-read-only))
                (:eval (custom-modeline-file-and-icon))
                (:eval (powerline-render (list
                                          (powerline-raw " " 'gray40-face)
                                          (powerline-arrow-left 'gray40-face 'gray17-face)
                                          (powerline-raw " " 'gray17-face))))

                (:eval (custom-modeline-vc))
                (:eval (powerline-render (list
                                          (powerline-raw " " 'gray17-face)
                                          (powerline-arrow-right 'gray17-face 'gray40-face)
                                          (powerline-raw " " 'gray40-face))))

                (:eval (custom-modeline-flycheck-status))))

(use-package moe-theme
  :demand t
  :config (moe-dark))

;; must be after moe-theme
(set-face-background 'mode-line "gray40")
(set-face-background 'mode-line-inactive "gray40")

(provide 'setup-theme)
;;; setup-theme.el ends here
