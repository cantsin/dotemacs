;;; setup-projectile -- Summary
;;; Commentary:
;;; Setup projectile.
;;; Code:
(require 'use-package)

(use-package projectile
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (projectile-global-mode)
  (setq projectile-switch-project-action 'projectile-find-file-dwim
        projectile-completion-system 'ivy
        projectile-switch-project-action 'counsel-projectile-find-file
        projectile-use-git-grep t))

(setq hydra-is-helpful t)
(setq hydra-lv t)
(setq lv-use-separator t)

(defun projectile-select-files (project-files &optional arg)
  "Select a list of files based on filename at point.

With a prefix ARG invalidates the cache first."
  (projectile-maybe-invalidate-cache arg)
  (let* ((file (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (or (thing-at-point 'filename) "")))
         ;; don't bother with relative paths in js2-mode
         (file (if (and (eq major-mode 'js2-mode) (string-match "\\.\\./" file))
                   (substring file 3) file))
         (file (if (and (eq major-mode 'js2-mode) (string-match "\\./" file))
                   (substring file 2) file))
         (file (if (string-match "\\.?\\./" file)
                   (file-relative-name (file-truename file) (projectile-project-root))
                 file))
         ;; if nothing is found in js2-mode, then append a 'lib/' after the first slash
         (file (if (and (eq major-mode 'js2-mode)
                        (string-match "/" file)
                        (eq nil (-filter (lambda (project-file)
                                           (string-match file project-file))
                                         project-files)))
                   (let* ((x (split-string file "/"))
                          (fst (car x))
                          (lst (cadr x)))
                     (mapconcat 'identity `(,fst "lib" ,lst) "/"))
                 file))
         (files (if file
                    (-filter (lambda (project-file)
                               (string-match file project-file))
                             project-files)
                  nil)))
    files))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
