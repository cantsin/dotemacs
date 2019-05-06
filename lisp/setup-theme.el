;;; setup-theme -- Summary
;;; Commentary:
;;; Setup theme modifications.
;;; Code:
(require 'use-package)

;; if icons do not show up:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package flycheck)

(use-package powerline)

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
    (propertize (format "%s" (all-the-icons-faicon "question-circle-o"))
                'face `(:foreground "LightGoldenrod" :background "gray17" :family ,(all-the-icons-faicon-family) :height 1.2)
                'display '(raise -0.1)))))

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
                (`not-checked "✖") ;; magit
                (`errored     "⚠")
                (`interrupted "⛔")
                (`suspicious  ""))))
     (propertize text
                 'help-echo "Show Flycheck Errors"
                 'mouse-face '(:box 1)
                 'face `(:foreground "gray90" :background "gray40")
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(defun custom-modeline-read-only ()
  "Customized modeline file status."
  (let* ((config-alist
          '(("%"
             all-the-icons-octicon-family
             all-the-icons-octicon
             "lock"
             :height 1.2 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (format "%s "
            (propertize (apply (cadr result) (cddr result))
                        'face `(:foreground "gray90" :family ,(funcall (car result)))
                        'display '(raise -0.1)))))

(defun custom-modeline-file-and-icon ()
  "Customized modeline file."
  (format "%s%s"
             (propertize (format "%s" (all-the-icons-icon-for-buffer))
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
                      'face `(:foreground "black" :background "LightGoldenrod" :weight bold))))

(defun powerline-arrow (where before after)
  "BEFORE color. AFTER color. WHERE direction. Draws an arrow."
  (format "%s%s%s"
          (propertize " "
                      'face `(:background ,before))
          (propertize (format "%s" (all-the-icons-alltheicon
                                    (concat "arrow-" (symbol-name where))))
                      'face `(:background ,(if (eq 'right where) after before)
                              :foreground ,(if (eq 'right where) before after)
                              :family ,(all-the-icons-alltheicon-family)
                              :box nil
                              :height 1.2)
                 'display '(raise -0.05))
          (propertize " "
                      'face `(:background ,after))))

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

(setq-default mode-line-format
              '("%e"
                (:eval (custom-modeline-location))
                (:eval (powerline-arrow 'right "LightGoldenrod" "gray17"))

                (:eval (custom-modeline-project))
                (:eval (powerline-arrow 'right "gray17" "gray40"))

                (:eval (custom-modeline-read-only))
                (:eval (custom-modeline-file-and-icon))
                (:eval (powerline-arrow 'right "gray40" "gray17"))

                (:eval (custom-modeline-vc))
                (:eval (powerline-arrow 'left "gray17" "gray40"))

                (:eval (custom-modeline-flycheck-status))))

(use-package moe-theme
  :ensure t
  :config (moe-dark))

(set-face-background 'mode-line "gray40")
(set-face-background 'mode-line-inactive "gray40")

;; TODO startup issue
;; TODO commit point

(provide 'setup-theme)
;;; setup-theme.el ends here
