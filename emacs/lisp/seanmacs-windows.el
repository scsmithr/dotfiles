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
         (sm/display-buffer-in-side-window-select)
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
         (display-buffer-in-side-window)
         (window-height . 12)
         (side . bottom)
         (slot . 0))
        ;; always open in same window
        ("\\*\
\\(shell\\).*"
         (display-buffer-same-window))))

(defun sm/display-buffer-in-side-window-select (buffer alist)
  "Display BUFFER in side window, selecting it."
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)))

(defun sm/pop-to-some-window ()
  "Display current buffer in some window, selecting it."
  (interactive)
  (let ((orig-win (get-buffer-window))
        (win (display-buffer-use-some-window (current-buffer) '())))
    (select-window win)
    (delete-window orig-win)))

(provide 'seanmacs-windows)
;;; seanmacs-windows.el ends here
