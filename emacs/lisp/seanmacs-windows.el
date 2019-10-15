;;; seanmacs-windows.el --- Window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window stuff.

;;; Code:

(core/leader
   "ww" 'seanmacs/split-window-right
   "wh" 'seanmacs/split-window-below
   "wH" 'shrink-window-horizontally
   "wL" 'enlarge-window-horizontally
   "wK" 'shrink-window
   "wJ" 'enlarge-window
   "w=" 'balance-windows
   "." 'repeat)

(defun seanmacs/split-window-right ()
  "Split window to the right, selecting it."
  (interactive)
  (let ((window (split-window-right)))
    (select-window window)))

(defun seanmacs/split-window-below ()
  "Split window below current, selecting it."
  (interactive)
  (let ((window (split-window-below)))
    (select-window window)))

(use-package winner
  :config
  (core/leader
   "wu" 'winner-undo)
  (winner-mode 1))

(provide 'seanmacs-windows)
;;; seanmacs-windows.el ends here
