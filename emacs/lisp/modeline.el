;; Mode line setup

(defgroup modeline nil
  "A minimal modeline configuration inspired by doom-modeline."
  :group 'modeline)

(defvar mode-line-bg (doom-color 'bg-alt))
(defvar mode-line-box `(:line-width 1 :color ,(doom-color 'base3) :style nil))

(set-face-attribute 'mode-line nil
    :foreground (doom-color 'fg) :background mode-line-bg
    :box mode-line-box)

(set-face-attribute 'mode-line-inactive nil
    :foreground (doom-color 'fg-alt) :background mode-line-bg
    :box mode-line-box)

(defcustom modeline-show-point nil
  "If t, the value of `point' will be displayed next to the cursor position in the modeline."
  :group 'modeline
  :type 'boolean)

(defface modeline-status-grayed-out
  '((t (:inherit (font-lock-doc-face))))
  "Face used for neutral or inactive status indicators in the modeline."
  :group 'modeline)

(defface modeline-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the modeline."
  :group 'modeline)

(defface modeline-status-success
  `((t (:inherit (success) :foreground ,(doom-color 'blue))))
  "Face used for success status indicators in the modeline."
  :group 'modeline)

(defface modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the modeline."
  :group 'modeline)

(defface modeline-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the modeline."
  :group 'modeline)

(defface modeline-unimportant
  '((t (:inherit (font-lock-doc-face))))
  "Face used for less important modeline elements."
  :group 'modeline)

(defface modeline-modified
  `((t (:inherit (error) :foreground ,(doom-color 'cyan))))
  "Face used for the 'modified' indicator symbol in the modeline."
  :group 'modeline)

(defun modeline-format (left right)
  (let ((reserve (length right)))
    (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (concat
     left
     " "
     (propertize " "
                 'display `((space :align-to (- (+ right right-margin) ,(+ reserve 0)))))
     right)))

;; Define a helper function to determine whether or not the current window is
;; active.
(defsubst modeline-is-active ()
  "Return \"t\" if the current window is active, \"nil\" if it is not."
  (eq (selected-window) modeline--current-window))

;; Window update function
(defvar-local modeline--current-window (frame-selected-window))
(defun modeline--update-selected-window (&rest _)
  "Update the `modeline--current-window' variable."
  (when (frame-selected-window)
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq modeline--current-window win)))))

;; VC update function
(defvar-local modeline--vc-text nil)
(defun modeline--update-vc-segment (&rest _)
  "Update `modeline--vc-text' against the current VCS state."
  (setq modeline--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'modeline-inactive)
                  (active (modeline-is-active)))
              (concat (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face 'modeline-status-info)
                      "  "))))))

;; Flycheck update function
(defvar-local modeline--flycheck-text nil)
(defun modeline--update-flycheck-segment (&optional status)
  "Update `modeline--flycheck-text' against the reported flycheck STATUS."
  (setq modeline--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'modeline-status-error
                                                 'modeline-status-warning))))
                       (propertize "Good  " 'face 'modeline-status-success)))
          ('running (propertize "Checking  " 'face 'modeline-status-info))
          ('no-checker "")
          ('errored (propertize "Error  " 'face 'modeline-status-error))
          ('interrupted (propertize "Paused  " 'face 'modeline-status-grayed-out)))))

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

(defun modeline-segment-modified-or-readonly ()
  "Displays a color-coded buffer modification or readonly
indicator in the modeline."
  (cond (buffer-read-only
           (propertize " R " 'face 'modeline-modified))
          ((buffer-modified-p)
           (propertize " * " 'face 'modeline-modified))
          (t "   ")))

(defun modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the modeline."
  (concat
   (propertize "%b" 'face 'mode-line-buffer-id) "  "))

(defun modeline-segment-dir ()
  "Display shortened working directory."
  (propertize (shorten-directory default-directory 15)))

(defun modeline-segment-position ()
  "Displays the current cursor position in the modeline."
  (concat "%l:%c"
          (when modeline-show-point
            (concat ":"
                    (propertize (format "%d" (point)) 'face (if (modeline-is-active)
                                                                'modeline-unimportant
                                                              'modeline-inactive))))
          " "
          (propertize "%p%%" 'face (if (modeline-is-active)
                                       'modeline-unimportant
                                     'modeline-inactive))
          "  "))

(defun modeline-segment-vc ()
  "Displays color-coded version control information in the modeline."
  modeline--vc-text)

(defun modeline-segment-major-mode ()
  "Displays the current major mode in the modeline."
  (propertize "%m "
              'face (if (modeline-is-active)
                        'bold
                      'modeline-status-grayed-out)))

(defun modeline-segment-flycheck ()
  "Displays color-coded flycheck information in the modeline (if available)."
  modeline--flycheck-text)

(defun modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the modeline."
  (when mode-line-process
    (list mode-line-process "  ")))

;; Store the default mode-line format
(defvar mood-line--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode modeline-mode
  "Toggle modeline on or off."
  :group 'modeline
  :global t
  :lighter nil
  (if modeline-mode
      (progn

        ;; Setup flycheck hooks
        (add-hook 'flycheck-status-changed-functions #'modeline--update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'modeline--update-flycheck-segment)

        ;; Setup VC hooks
        (add-hook 'find-file-hook #'modeline--update-vc-segment)
        (add-hook 'after-save-hook #'modeline--update-vc-segment)
        (advice-add #'vc-refresh-state :after #'modeline--update-vc-segment)

        ;; Setup window update hooks
        (add-hook 'window-configuration-change-hook #'modeline--update-selected-window)
        (add-hook 'focus-in-hook #'modeline--update-selected-window)
        (advice-add #'handle-switch-frame :after #'modeline--update-selected-window)
        (advice-add #'select-window :after #'modeline--update-selected-window)

        ;; Set the new modeline-format
        (setq-default mode-line-format
                      '((:eval
                         (modeline-format
                          ;; Left
                          (format-mode-line
                           '((:eval (modeline-segment-modified-or-readonly))
                             (:eval (modeline-segment-dir))
                             (:eval (modeline-segment-buffer-name))
                             (:eval (modeline-segment-anzu))
                             (:eval (modeline-segment-multiple-cursors))
                             (:eval (modeline-segment-position))))

                          ;; Right
                          (format-mode-line
                           '((:eval (modeline-segment-flycheck))
                             (:eval (modeline-segment-vc))
                             (:eval (modeline-segment-major-mode))
                             (:eval (modeline-segment-global-mode-string))
                             (:eval (modeline-segment-process))
                             " ")))))))
    (progn

      ;; Remove flycheck hooks
      (remove-hook 'flycheck-status-changed-functions #'modeline--update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'modeline--update-flycheck-segment)

      ;; Remove VC hooks
      (remove-hook 'file-find-hook #'modeline--update-vc-segment)
      (remove-hook 'after-save-hook #'modeline--update-vc-segment)
      (advice-remove #'vc-refresh-state #'modeline--update-vc-segment)

      ;; Remove window update hooks
      (remove-hook 'window-configuration-change-hook #'modeline--update-selected-window)
      (remove-hook 'focus-in-hook #'modeline--update-selected-window)
      (advice-remove #'handle-switch-frame #'modeline--update-selected-window)
      (advice-remove #'select-window #'modeline--update-selected-window)

      ;; Restore the original modeline format
      (setq-default modeline-format modeline--default-modeline))))

(provide 'modeline)

