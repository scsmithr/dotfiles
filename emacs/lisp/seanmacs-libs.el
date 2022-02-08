;;; seanmacs-libs.el --- Functions/libraries -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions and libraries that should be loaded early on, and can be used
;; elsewhere within my configuration.

;;; Code:

(require 'cl-lib)

;; Useful string utilities, e.g. 's-contains-p'.
(use-package s :straight t)

(use-package dash :straight t)

(defun sm/run-and-bury (fn &rest args)
  "Run FN with ARGS then bury the buffer."
  (let ((buf (buffer-name)))
    (apply fn args)
    (bury-buffer buf)))

(defun sm/unfill-paragraph (beg end)
  "Turn a paragraph into a single line of text."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph nil))))

(define-key global-map (kbd "M-Q") #'sm/unfill-paragraph)

(defun sm/indent-region-or-buffer (start end)
  "Indent region from START to END, or the entire buffer if no region is selected."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (indent-region start end))

(define-key global-map (kbd "C-c b i") #'sm/indent-region-or-buffer)
(define-key global-map (kbd "C-c .") #'repeat)

(defun sm/ansi-colorize (beg end)
  "Colorize region according to ANSI control sequences from BEG to END.
If no region selected, colorize the entire buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))

;; Tramp and filepath helpers

(defun sm/path-localname (path)
  "Return the localname of PATH if it's a remote file, or just PATH otherwise."
  (if (file-remote-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))

;; Testing helpers

(defvar sm/test-root "~/dotfiles/emacs/test/"
  "Directory containing test files.")

(defun sm/load-tests ()
  "Load test files from `sm/test-root'."
  (interactive)
  (mapcar #'load-file (directory-files sm/test-root t ".el$")))

(defun sm/run-all-tests ()
  "Load and run all tests."
  (interactive)
  (sm/load-tests)
  (ert t))

;; Kill ring helpers

(defun sm/kill-ring-alist (kill-ring)
  "Create an alist of (CLEANED . ORIGINAL) for a KILL-RING."
  (let ((alist))
    (dolist (item kill-ring)
      (push (cons (sm/clean-kill-item item) item) alist))
    (reverse alist)))

(defun sm/clean-kill-item (item)
  "Clean ITEM for use in a single line context."
  (set-text-properties 0 (length item) nil item)
  (let ((replace-fn (lambda (regexp rep string)
                      (replace-regexp-in-string regexp rep string nil t))))
    (let ((cleaned (thread-last item
                     (funcall replace-fn "\n" "\\n")
                     (funcall replace-fn "^\t*" "")
                     (funcall replace-fn "\t" "\\t")
                     (funcall replace-fn "^[ ]*" "")))
          (truncate-width 80))
      (if (> (length cleaned) truncate-width)
          (truncate-string-to-width cleaned truncate-width 0 0 "...")
        cleaned))))

(defun sm/insert-from-kill-ring-at-point ()
  "Select an item from the kill ring and insert it at point."
  (interactive)
  (let ((selectrum-should-sort nil) ;; Keep order of kill-ring
        (alist (sm/kill-ring-alist kill-ring)))
    (let ((selected (completing-read "Item: " alist nil t)))
      (when-let ((entry (assoc selected alist)))
        (insert (cdr entry))))))

;; Misc

(defun sm/scratch ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (and initial-major-mode (funcall initial-major-mode))
    (insert initial-scratch-message)))

(defun sm/warn-fn-not-bound (fn-symbol)
  "Warn if FN-SYMBOL is void."
  (when (not (fboundp fn-symbol))
    (message "WARN: %s is void!" fn-symbol)))

(defmacro sm/save-window-excursion (&rest body)
  "Save active window, execute BODY, then restore the originally active window.

Same peculiarities as with `save-window-excursion'."
  (let ((win (gensym)))
    `(let ((,win (selected-window)))
       (progn
         ,@body
         (recenter)
         (pulse-momentary-highlight-one-line (point))
         (select-window ,win)))))

(provide 'seanmacs-libs)
;;; seanmacs-libs.el ends here

