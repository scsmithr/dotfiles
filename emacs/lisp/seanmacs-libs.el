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

;; Helpers for downloading things

(defun sm/download-pdf (link &optional type open)
  "Download and save a pdf from LINK.

TYPE may be one of paper, book, arxiv, or refile. When OPEN is
non-nil, open the downloaded pdf. Does not overwrite existing
files."
  (interactive (list (read-string "Link: ")
                     (completing-read "Type: " '(paper book arxiv refile))
                     t))
  (let ((dir (cond ((string= type 'paper) "~/syncthing/papers/")
                   ((string= type 'book)  "~/syncthing/books/")
                   ((string= type 'arxiv) "~/syncthing/arxiv/downloads/")
                   (t                     "~/syncthing/refile/")))
        (name (file-name-nondirectory link)))
    (mkdir dir t)
    (if (string-empty-p name)
        (user-error "Got empty file name from link: %s" link)
      (let ((path (concat dir (if (string-suffix-p ".pdf" name) name (concat name ".pdf")))))
        (if (file-exists-p path)
            (message "File already exists at %s" path)
          (with-current-buffer (url-retrieve-synchronously link)
            (write-region nil nil path)))
        (when open (find-file path))))))

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

