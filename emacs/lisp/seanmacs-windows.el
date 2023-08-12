;;; seanmacs-windows.el --- Window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window stuff.

;;; Code:

(setq split-height-threshold 100
      split-width-threshold 160)

(setq display-buffer-alist
      '(;; selecting bottom side window
        ("\\*\
\\(Flymake diagnostics\
\\|xref\
\\|docker\
\\).*"
         (sm/display-buffer-in-side-window-or-reuse-select)
         (window-height . 12)
         (side . bottom)
         (slot . 1))
        ;; bottom side window
        ("\\*\
\\(Completions\
\\|Help\
\\|Warnings\
\\|eglot-help\
\\).*"
         (sm/display-buffer-in-side-window-or-reuse)
         (window-height . 12)
         (side . bottom)
         (slot . 0))
        ;; always open in same window
        ("\\*\
\\(shell\\).*"
         (display-buffer-same-window))))

(defun sm/display-buffer-in-side-window-or-reuse (buffer alist &optional select)
  "Display BUFFER in a side window, or reuse an already visible window.

ALIST is passed to the underlying display buffer function. If
SELECT is non-nil, the window will be selected."
  (let* ((reused (display-buffer-reuse-window buffer alist))
         (win (if reused
                  reused
                (display-buffer-in-side-window buffer alist))))
    (when select
      (select-window win))))

(defun sm/display-buffer-in-side-window-or-reuse-select (buffer alist)
  "Display BUFFER ins side window, or reuse an already visible window.

ALIST will be passed to the underlying display buffer function.
The window displaying the buffer will be automatically selected."
  (sm/display-buffer-in-side-window-or-reuse buffer alist t))

(defun sm/pop-to-some-window ()
  "Display current buffer in some window, selecting it."
  (interactive)
  (let ((orig-win (get-buffer-window))
        (win (display-buffer-use-some-window (current-buffer) nil)))
    (select-window win)
    (delete-window orig-win)))

(defun sm/swap-window (window direction)
  "Swap WINDOW with other window in DIRECTION.

This will never attempt to swap the window with the minibuffer,
or dedicated windows."
  (let ((neighbor (window-in-direction direction window nil nil nil 'never)))
    (unless neighbor
      (user-error "No window in direction: %s" direction))
    (when (or (window-dedicated-p neighbor)
              (window-dedicated-p window))
      (user-error "Cannot swap dedicated windows"))
    (window-swap-states window neighbor)))

(defun sm/swap-window-above ()
  "Swap current window above."
  (interactive)
  (sm/swap-window (selected-window) 'above))

(defun sm/swap-window-below ()
  "Swap current window below."
  (interactive)
  (sm/swap-window (selected-window) 'below))

(defun sm/swap-window-left ()
  "Swap current window left."
  (interactive)
  (sm/swap-window (selected-window) 'left))

(defun sm/swap-window-right ()
  "Swap current window right."
  (interactive)
  (sm/swap-window (selected-window) 'right))

(provide 'seanmacs-windows)
;;; seanmacs-windows.el ends here
