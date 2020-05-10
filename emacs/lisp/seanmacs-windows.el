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
 "wp" 'seanmacs/use-some-window
 "." 'repeat

 "dp" 'seanmacs/window-dired-project-left)

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

(defun seanmacs/window-dired-project-left ()
  "Open a dired at the root of the current project in the left frame."
  (interactive)
  (let* ((root (projectile-project-root))
         (dir (if root root default-directory))
         (buf-name (format "*Dired Side:%s*" dir))
         (buf (if (get-buffer buf-name)
                  (get-buffer buf-name)
                (dired-noselect dir))))
    (display-buffer-in-side-window
     buf `((side . left)
           (slot . -1)
           (window-width . 25)
           (window-parameters . ((mode-line-format . (" "
                                                      mode-line-buffer-identification))))))
    (with-current-buffer buf
      (dired-hide-details-mode 1)
      (rename-buffer buf-name)
      (setq-local window-size-fixed 'width))))

(use-package winner
  ;; built-in
  :config
  (core/leader
   "wu" 'winner-undo)
  (winner-mode 1))

(use-package window
  ;; built-in
  :defer t
  :init
  (setq display-buffer-alist
        '(;; selecting bottom side window
          ("\\*\
\\(Flycheck\
\\|ripgrep-search\
\\|docker\
\\|kube\
\\|xref\
\\).*"
           (seanmacs/display-buffer-in-side-window-select)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ;; bottom side window
          ("\\*\
\\(Completions\
\\|Ido Completions\
\\|Help\
\\|lsp-help\
\\|tide-documentation\
\\|company-documentation\
\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ;; always open in same window
          ("\\*\
\\(shell\\).*"
           (display-buffer-same-window)))))

(defun seanmacs/display-buffer-in-side-window-select (buffer alist)
  "Display BUFFER in side window, selecting it."
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)))

(defun seanmacs/use-some-window ()
  (interactive)
  (let ((buf (current-buffer)))
    (with-current-buffer buf
      (delete-window)
      (display-buffer-use-some-window buf '()))))

(provide 'seanmacs-windows)
;;; seanmacs-windows.el ends here
