;; Mode line setup
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " R " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " * " 'face 'mode-line-modified-face))
          (t "   ")))
   "  "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; mode indicators: vc, recursive edit, major mode, minor modes, process,
   ; global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground (doom-color 'fg) :background (doom-color 'base0)
    :inverse-video nil
    :box '(:line-width 6 :color "#1B2229" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground (doom-color 'fg-alt) :background (doom-color 'base0)
    :inverse-video nil
    :box '(:line-width 6 :color "#1B2229" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'orange))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'cyan))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'fg-alt))
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'blue)
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'fg))
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground (doom-color 'fg-alt))
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'fg-alt))
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground (doom-color 'orange))

