;; Mode line setup
(setq-default
 mode-line-format
 '(
   " "
   (:propertize (:eval (format "%s" (line-number-at-pos (point-max)))) face mode-line-position-face)
   " "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 15))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   " "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize "R" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "*" 'face 'mode-line-modified-face))
          (t " ")))
   " "
   ;; Flycheck errors
   (:eval
       (when (and (bound-and-true-p flycheck-mode)
                  (or flycheck-current-errors
                      (eq 'running flycheck-last-status-change)))
         (concat
          (cl-loop for state in '((error . mode-line-flycheck-error-face)
                                  (warning . mode-line-flycheck-warning-face)
                                  (info . mode-line-flycheck-info-face))
                   as lighter = (d/flycheck-lighter (car state))
                   when lighter
                   concat (propertize
                           lighter
                           'face (cdr state)))
          " ")))
   " "
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   " "
   (:eval (propertize
         " " 'display
         `((space :align-to (- (+ right right-fringe right-margin)
                               ,(+ 2 (string-width (concat vc-mode mode-name))))))))
   (:propertize (vc-mode vc-mode) face mode-line-vc-face)
   " "
   (:propertize mode-name
                face mode-line-mode-face)
))

(defun d/flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) " "))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format " %s" err))))

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
(make-face 'mode-line-flycheck-error-face)
(make-face 'mode-line-flycheck-warning-face)
(make-face 'mode-line-flycheck-info-face)

(set-face-attribute 'mode-line-flycheck-error-face nil
    :inherit 'mode-line-face
    :weight 'bold
    :foreground (doom-color 'red))

(set-face-attribute 'mode-line-flycheck-warning-face nil
    :inherit 'mode-line-face
    :weight 'bold
    :foreground (doom-color 'orange))

(set-face-attribute 'mode-line-flycheck-info-face nil
    :inherit 'mode-line-face
    :weight 'bold
    :foreground (doom-color 'green))

(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-vc-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(defvar mode-line-bg (doom-color 'base0))
(defvar mode-line-box `(:line-width 6 :color ,mode-line-bg :style nil))

(set-face-attribute 'mode-line nil
    :foreground (doom-color 'fg) :background mode-line-bg
    :inverse-video nil
    :box mode-line-box)

(set-face-attribute 'mode-line-inactive nil
    :foreground (doom-color 'fg-alt) :background mode-line-bg
    :inverse-video nil
    :box mode-line-box)

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'blue))

(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :weight 'bold
    :foreground (doom-color 'cyan))

(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'fg-alt))

(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :weight 'bold)

(set-face-attribute 'mode-line-vc-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'green))

(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :foreground (doom-color 'fg-alt))

(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground (doom-color 'fg-alt))

(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground (doom-color 'fg-alt))

(provide 'modeline)
