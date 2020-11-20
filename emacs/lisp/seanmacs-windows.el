;;; seanmacs-windows.el --- Window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window stuff.

;;; Code:

(defun sm/split-window-right ()
  "Split window to the right, selecting it."
  (interactive)
  (let ((window (split-window-right)))
    (select-window window)))

(defun sm/split-window-below ()
  "Split window below current, selecting it."
  (interactive)
  (let ((window (split-window-below)))
    (select-window window)))

(defun sm/dired-project-sidebar ()
  "Open a dired at the root of the current project in the left frame."
  (interactive)
  (let* ((root (projectile-project-root))
         (dir (if root root default-directory))
         (buf-name (format "*Dired Project:%s*" dir))
         (buf (if (get-buffer buf-name)
                  (get-buffer buf-name)
                (dired-noselect dir))))
    (sm/display-buffer-in-side-window-select
     buf `((side . left)
           (slot . -1)
           (window-width . 25)
           (window-parameters . ((mode-line-format . (" "
                                                      mode-line-buffer-identification))))))
    (with-current-buffer buf
      (dired-unadvertise dir)
      (dired-hide-details-mode 1)
      (rename-buffer buf-name)
      (setq-local window-size-fixed 'width))))

(defun sm/rename-buffer-special (newname)
  "Rename buffer to NEWNAME, wrapping NEWNAME in '*' characters when original name has them."
  (interactive (list (read-string "Rename buffer (to new name): ")))
  (let ((newname (if (string-prefix-p "*" (buffer-name))
                     (format "*%s*" newname)
                   newname)))
    (rename-buffer newname t)))

(defun sm/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(use-package winner
  ;; built-in
  :config
  (winner-mode 1))

(use-package window
  ;; built-in
  :defer t
  :init
  (setq display-buffer-alist
        '(;; selecting bottom side window
          ("\\*\
\\(Flycheck errors\
\\|ripgrep-search\
\\|grep\
\\|docker\
\\|kube\
\\|xref\
\\).*"
           (sm/display-buffer-in-side-window-select)
           (window-height . 0.25)
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
\\|tide-documentation\
\\|company-documentation\
\\|Gofmt Errors\
\\|prettier errors\
\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . bottom)
           (slot . 0))
          ;; always open in same window
          ("\\*\
\\(shell\\).*"
           (display-buffer-same-window)))))

(defun sm/display-buffer-in-side-window-select (buffer alist)
  "Display BUFFER in side window, selecting it."
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)))

(defun sm/use-some-window ()
  (interactive)
  (let ((buf (current-buffer)))
    (with-current-buffer buf
      (delete-window)
      (display-buffer-use-some-window buf '()))))

(provide 'seanmacs-windows)
;;; seanmacs-windows.el ends here
