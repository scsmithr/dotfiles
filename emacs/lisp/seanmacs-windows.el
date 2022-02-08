;;; seanmacs-windows.el --- Window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window stuff.

;;; Code:

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

(provide 'seanmacs-windows)
;;; seanmacs-windows.el ends here
