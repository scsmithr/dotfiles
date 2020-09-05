;;; seanmacs-modeline.el --- Modeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Modeline configuration.

;;; Code:

(defgroup seanmacs/modeline nil
  "A minimal modeline configuration inspired by doom-modeline."
  :group 'modeline)

(defface seanmacs/modeline-status-mode
  `((t (:inherit (font-lock-keyword-face))))
  "Face used for mode indicators in the modeline."
  :group 'seanmacs/modeline)

(defface seanmacs/modeline-status-vc
  `((t (:inherit (font-lock-builtin-face))))
  "Face used for vc indicators in the modeline."
  :group 'seanmacs/modeline)

(defface seanmacs/modeline-status-success
  `((t (:inherit (success))))
  "Face used for success status indicators in the modeline."
  :group 'seanmacs/modeline)

(defface seanmacs/modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the modeline."
  :group 'seanmacs/modeline)

(defface seanmacs/modeline-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the modeline."
  :group 'seanmacs/modeline)

(defun seanmacs/modeline-format (left right)
  "Format the modeling, aligning LEFT and RIGHT."
  (let ((reserve (length right)))
    (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (concat
     left
     " "
     (propertize " "
                 'display `((space :align-to (- (+ right right-margin) ,(+ reserve 0)))))
     right)))

;; Window update function
(defvar-local seanmacs/modeline-current-window (frame-selected-window))
(defun seanmacs/modeline-update-selected-window (&rest _)
  "Update the `seanmacs/modeline-current-window' variable."
  (when (frame-selected-window)
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq seanmacs/modeline-current-window win)))))

;; VC update function
(defvar-local seanmacs/modeline-vc-text nil)
(defun seanmacs/modeline-update-vc-segment (&rest _)
  "Update `seanmacs/modeline-vc-text' against the current VCS state."
  (setq seanmacs/modeline-vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name)))
            (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))))

;; Flycheck update function
(defvar-local seanmacs/modeline-flycheck-text nil)
(defun seanmacs/modeline-update-flycheck-segment (&optional status)
  "Update `seanmacs/modeline-flycheck-text' against the reported flycheck STATUS."
  (setq seanmacs/modeline-flycheck-text
        (pcase status
          (`finished (let-alist (flycheck-count-errors flycheck-current-errors)
                       (let ((error (or .error 0))
                             (warning (or .warning 0))
                             (info (or .info 0)))
                         (format "%s/%s/%s "
                                 (if (> error 0)
                                     (propertize (number-to-string error) 'face 'seanmacs/modeline-status-error)
                                   (number-to-string error))
                                 (if (> warning 0)
                                     (propertize (number-to-string warning) 'face 'seanmacs/modeline-status-warning)
                                   (number-to-string warning))
                                 (if (> info 0)
                                     (propertize (number-to-string info) 'face 'seanmacs/modeline-status-success)
                                   (number-to-string info))))))
          ('running "-/-/- ")
          ('no-checker "")
          ('errored (propertize "!!! " 'face 'seanmacs/modeline-status-error))
          ('interrupted "--- "))))

(defun seanmacs/modeline-segment-modified-or-readonly ()
  "Displays a color-coded buffer modification or readonly
indicator in the modeline."
  (cond (buffer-read-only
         "R ")
        ((buffer-modified-p)
         "U ")
        (t "  ")))

(defun seanmacs/modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the modeline."
  (propertize "%b " 'face 'mode-line-buffer-id))

(defun seanmacs/modeline-segment-dir ()
  "Display shortened working directory."
  (when default-directory
    (shrink-path-dirs default-directory)))

(defun seanmacs/modeline-segment-position ()
  "Displays the current cursor position in the modeline."
  (let ((fmt-string " %3l:%2c "))
    fmt-string))

(defun seanmacs/modeline-segment-buffer-percent ()
  "Displays the percentage of buffer above current point."
  (let ((fmt-string "%p%% "))
    fmt-string))

(defun seanmacs/modeline-segment-vc ()
  "Displays color-coded version control information in the modeline."
  (when seanmacs/modeline-vc-text
    (propertize seanmacs/modeline-vc-text 'face 'seanmacs/modeline-status-vc)))

(defun seanmacs/modeline-segment-major-mode ()
  "Displays the current major mode in the modeline."
  (propertize " %m" 'face 'seanmacs/modeline-status-mode))

(defun seanmacs/modeline-segment-flycheck ()
  "Displays color-coded flycheck information in the modeline (if available)."
  seanmacs/modeline-flycheck-text)

(defun seanmacs/modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the modeline."
  (when mode-line-process
    (list mode-line-process)))

(defvar seanmacs/modeline-default-modeline nil
  "Holds the original modeline format.")

;;;###autoload
(define-minor-mode seanmacs/modeline-mode
  "Toggle modeline on or off."
  :group 'seanmacs/modeline
  :global t
  :lighter nil
  (if seanmacs/modeline-mode
      (progn
        ;; Store original modeline format.
        (setq seanmacs/modeline-default-modeline mode-line-format)

        ;; Setup flycheck hooks
        (add-hook 'flycheck-status-changed-functions #'seanmacs/modeline-update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'seanmacs/modeline-update-flycheck-segment)

        ;; Setup VC hooks
        (add-hook 'find-file-hook #'seanmacs/modeline-update-vc-segment)
        (add-hook 'after-save-hook #'seanmacs/modeline-update-vc-segment)
        (advice-add #'vc-refresh-state :after #'seanmacs/modeline-update-vc-segment)

        ;; Setup window update hooks
        (add-hook 'window-configuration-change-hook #'seanmacs/modeline-update-selected-window)
        (add-hook 'focus-in-hook #'seanmacs/modeline-update-selected-window)
        (advice-add #'handle-switch-frame :after #'seanmacs/modeline-update-selected-window)
        (advice-add #'select-window :after #'seanmacs/modeline-update-selected-window)

        ;; Set the new modeline-format
        (setq-default mode-line-format
                      '((:eval
                         (seanmacs/modeline-format
                          ;; Left
                          (format-mode-line
                           '((:eval (seanmacs/modeline-segment-position))
                             (:eval (seanmacs/modeline-segment-modified-or-readonly))
                             (:eval (seanmacs/modeline-segment-dir))
                             (:eval (seanmacs/modeline-segment-buffer-name))
                             (:eval (seanmacs/modeline-segment-buffer-percent))))

                          ;; Right
                          (format-mode-line
                           '((:eval (seanmacs/modeline-segment-flycheck))
                             (:eval (seanmacs/modeline-segment-vc))
                             (:eval (seanmacs/modeline-segment-major-mode))
                             (:eval (seanmacs/modeline-segment-process))
                             " ")))))))
    (progn

      ;; Remove flycheck hooks
      (remove-hook 'flycheck-status-changed-functions #'seanmacs/modeline-update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'seanmacs/modeline-update-flycheck-segment)

      ;; Remove VC hooks
      (remove-hook 'file-find-hook #'seanmacs/modeline-update-vc-segment)
      (remove-hook 'after-save-hook #'seanmacs/modeline-update-vc-segment)
      (advice-remove #'vc-refresh-state #'seanmacs/modeline-update-vc-segment)

      ;; Remove window update hooks
      (remove-hook 'window-configuration-change-hook #'seanmacs/modeline-update-selected-window)
      (remove-hook 'focus-in-hook #'seanmacs/modeline-update-selected-window)
      (advice-remove #'handle-switch-frame #'seanmacs/modeline-update-selected-window)
      (advice-remove #'select-window #'seanmacs/modeline-update-selected-window)

      ;; Restore the original modeline format
      (setq-default mode-line-format seanmacs/modeline-default-modeline))))

(provide 'seanmacs-modeline)
;;; seanmacs-modeline.el ends here

