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

;; Misc

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

