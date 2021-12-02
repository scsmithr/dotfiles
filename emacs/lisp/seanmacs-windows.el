;;; seanmacs-windows.el --- Window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window stuff.

;;; Code:

(defun sm/rename-buffer-special (newname)
  "Rename buffer to NEWNAME, wrapping NEWNAME in '*' characters when original name has them."
  (interactive (list (read-string "Rename buffer (to new name): ")))
  (let ((newname (if (string-prefix-p "*" (buffer-name))
                     (format "*%s*" newname)
                   newname)))
    (rename-buffer newname t)))

(defun sm/indent-region-or-buffer (start end)
  "Indent region from START to END, or the entire buffer if no region is selected."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (indent-region start end))

(use-package winner
  ;; built-in
  :config
  (winner-mode 1))

(use-package window
  ;; built-in
  :init
  (setq display-buffer-alist
        '(;; selecting bottom side window
          ("\\*\
\\(Flycheck errors\
\\|kube\
\\|xref\
\\|docker\
\\).*"
           (sm/display-buffer-in-side-window-select)
           (window-height . 0.3)
           (side . bottom)
           (slot . 1))
          ;; bottom side window
          ("\\*\
\\(Completions\
\\|Flycheck error messages\
\\|Ido Completions\
\\|Help\
\\|Warnings\
\\|lsp-help\
\\|eglot-help\
\\|company-documentation\
\\|Gofmt Errors\
\\|prettier errors\
\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.3)
           (side . bottom)
           (slot . 0))
          ;; always open in same window
          ("\\*\
\\(shell\\).*"
           (display-buffer-same-window))))
  :bind (("C-c ." . repeat)
         ("C-c b i" . sm/indent-region-or-buffer)
         ("C-c b r" . sm/rename-buffer-special)))

(defun sm/display-buffer-in-side-window-select (buffer alist)
  "Display BUFFER in side window, selecting it."
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)))

(provide 'seanmacs-windows)
;;; seanmacs-windows.el ends here
