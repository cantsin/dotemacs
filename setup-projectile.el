;;; setup-projectile -- Summary
;;; Commentary:
;;; Setup projectile.
;;; Code:
(require 'use-package)

(setq hydra-is-helpful t)
(setq hydra-lv t)
(setq lv-use-separator t)
(setq projectile-switch-project-action 'projectile-find-file-dwim)

(defhydra hydra-window (global-map "C-x o"
                        :idle 0.0
                        :color red)
  "
     Create               Delete              Move                     Misc
------------------------------------------------------------------------------------------
_|_: vertical          _o_: only           _s_: swap                _b_: balance
_: horizontal        _d_: window
_N_: new frame         _f_: frame
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" flop-frame)
  ("o" delete-other-windows :exit t)
  ("N" make-frame :exit t)
  ("b" balance-windows)
  ("d" delete-window)
  ("f" delete-frame :exit t)
  ("q" nil))

(defhydra hydra-projectile (:idle 0.0
                            :color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
