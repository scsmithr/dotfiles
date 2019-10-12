;;; seanmacs-windows.el --- Window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window stuff.

;;; Code:

(core/leader
   "ww" 'split-window-right
   "wh" 'split-window-below
   "wH" 'shrink-window-horizontally
   "wL" 'enlarge-window-horizontally
   "wK" 'shrink-window
   "wJ" 'enlarge-window
   "w=" 'balance-windows
   "." 'repeat)

(use-package winner
  :straight (winner :type built-in)
  :config
  (core/leader
   "wu" 'winner-undo)
  (winner-mode 1))

(provide 'seanmacs-windows)
;;; seanmacs-windows.el ends here
