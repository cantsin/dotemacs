(require 'magit)

(set-face-foreground 'diff-context "#666666")
(set-face-foreground 'diff-added "#00cc33")
(set-face-foreground 'diff-removed "#ff0000")

(global-set-key (kbd "C-c g") 'magit-status)

(provide 'setup-magit)
