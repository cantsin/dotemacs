;;; setup-unity -- Summary
;;; Commentary:
;;; Setup unity Emacs usage on windows.
;;; Code:

;(require 'shaderlab-mode)
(require 'flycheck)
(require 'dash)
(require 'rx)
(require 'auto-complete)

(defun forward-slashes-to-backslashes (str)
  "Windows conversion of slashes in STR."
  (replace-regexp-in-string "/" "\\" str t t))

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'csharp-mode-hook (lambda ()
;;                               (add-to-list 'ac-sources 'ac-source-omnisharp)))
(add-hook 'csharp-mode-hook 'auto-complete-mode)

(require 'omnisharp)
(setq omnisharp--windows-curl-tmp-file-path "c:/Users/james/omnisharp-tmp-file.cs")

(add-to-list 'ido-ignore-files "\\.meta$")

(defun my-compilation-mode-hook ()
  "Make compile errors easier to read."
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun unity-find-project-sln-from-dir (dir-path)
  "Walks up the tree from DIR-PATH, trying to find an sln.
If it finds one, returns it, else nil."
  (if (and (file-exists-p dir-path) (not (string= dir-path "/")))
      (let* (
            (dir-name (file-name-base (directory-file-name dir-path)))
            (sln-path (concat (file-name-as-directory dir-path)
                              (concat dir-name ".sln"))))
        (if (file-exists-p sln-path)
            sln-path
          (unity-find-project-sln-from-dir (expand-file-name (concat dir-path "/.."))) ;; keep going down stack
          ))
    nil))

(defun buffer-has-unity-sln-parent ()
  "Find the sln file that defines this project."
  (unity-find-project-sln-from-dir (file-name-directory buffer-file-name)))

(flycheck-define-checker unity-csharp-mdtool-flychecker
  "given a c-sharp file, looks for the unity file and then tries to build it using mdtool."

  :command ("C:/Program Files (x86)/Xamarin Studio/bin/mdtool.exe"
            "build"
            (eval (forward-slashes-to-backslashes
                   (unity-find-project-sln-from-dir (file-name-directory buffer-file-name)))))

  :error-patterns
  ((error   line-start
            (file-name (1+ (in ":" "\\" alnum)) ".cs")
            "(" line "," column ")"
            " : error "
            (message (0+ any))
            line-end)
   (warning line-start
            (file-name (1+ (in ":" "\\" alnum)) ".cs")
            "(" line "," column ")"
            " : warning "
            (message (0+ any))
            line-end))

  :modes csharp-mode

  ;; checker only valid if we can find an sln
  :predicate (lambda () (buffer-has-unity-sln-parent)))
(add-to-list 'flycheck-checkers 'unity-csharp-mdtool-flychecker 'append)

(provide 'setup-unity)
;;; setup-unity.el ends here
