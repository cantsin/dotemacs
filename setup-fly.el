;;; setup-fly -- Summary
;;; Commentary:
;;; Setup fly{spell,check}
;;; Code:

(require 'use-package)

;; flyspell should be able to scan previous errors as well
(defun flyspell-goto-previous-error (&optional arg)
  "Count ARG mis-spelled words backwards."
  (interactive)
  (let ((pos1 (point))
	(pos  (point))
	(arg  (if (or (not (numberp arg)) (< arg 1)) 1 arg))
	ov ovs)
    (if (catch 'exit
	  (while (and (setq pos (previous-overlay-change pos))
		      (not (= pos pos1)))
	    (setq pos1 pos)
	    (if (> pos (point-min))
		(progn
		  (setq ovs (overlays-at (1- pos)))
		  (while (consp ovs)
		    (setq ov (car ovs))
		    (setq ovs (cdr ovs))
		    (if (and (flyspell-overlay-p ov)
			     (= 0 (setq arg (1- arg))))
			(throw 'exit t)))))))
        (progn
         (goto-char pos)
         (backward-word))
      (message "No word to correct before point"))))

;; combine flyspell and flycheck with the same command
(defun fly-display-error-or-next-error ()
  "Display information for current error, or go to next one."
  (interactive)
  (when (or (not (flycheck-overlay-errors-at (point)))
            (not (flyspell-overlay-p (point))))
    (fly-goto-next-error)))

(defun fly-goto-next-error ()
  "Jump to next flyspell or flycheck error."
  (interactive)
  (let* ((p (point))
         (message-log-max nil)
         (spell-next-error-function '(lambda ()
                                       (forward-word) (forward-char)
                                       (flyspell-goto-next-error)))
         (spell-pos (save-excursion
                      (funcall spell-next-error-function)
                      (point)))
         (make-pos (save-excursion
                     (flycheck-next-error)
                     (point))))
    (cond ((or (and (< p make-pos) (< p spell-pos))
               (and (> p make-pos) (> p spell-pos)))
           (funcall (if (< make-pos spell-pos)
                        'flycheck-next-error
                      spell-next-error-function)))
          ((< p make-pos)
           (flycheck-next-error))
          ((< p spell-pos)
           (funcall spell-next-error-function)))))

(defun fly-goto-previous-error ()
  "Jump to previous flyspell or flycheck error."
  (interactive)
  (let* ((p (point))
         (message-log-max nil)
         (spell-previous-error-function '(lambda ()
                                       (backward-char)
                                       (flyspell-goto-previous-error)))
         (spell-pos (save-excursion
                      (funcall spell-previous-error-function)
                      (point)))
         (make-pos (save-excursion
                     (flycheck-previous-error)
                     (point))))
    (cond ((or (and (< p make-pos) (< p spell-pos))
               (and (> p make-pos) (> p spell-pos)))
           (funcall (if (< make-pos spell-pos)
                        'flycheck-previous-error
                      spell-previous-error-function)))
          ((< p make-pos)
           (flycheck-previous-error))
          ((< p spell-pos)
           (funcall spell-previous-error-function)))))

(defun cantsin/flyspell-setup ()
  "Deferred setup of 'flyspell-mode'."
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package flyspell
  :ensure t
  :defer t
  :bind (("C-c s" . flyspell-correct-word-before-point)
         ("M-g n" . fly-goto-next-error)
         ("M-g p" . fly-goto-previous-error))
  :init (cantsin/flyspell-setup)
  :config (setq ispell-program-name "/usr/bin/aspell")
  :diminish flyspell-mode)

(defun cantsin/flycheck-setup ()
  "Deferred setup of 'flycheck-mode'."
  (global-flycheck-mode t)
  (add-hook 'emacs-lisp-mode-hook
            (function (lambda ()
                        (setq flycheck-emacs-lisp-load-path load-path))))
  (use-package flycheck-color-mode-line
    :ensure t
    :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package flycheck
  :ensure t
  :defer t
  :commands global-flycheck-mode
  :init (cantsin/flycheck-setup))

(provide 'setup-fly)
;;; setup-fly.el ends here
