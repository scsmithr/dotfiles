;;; seanmacs-libs.el --- Functions/libraries -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions and libraries that should be loaded early on, and can be used
;; elsewhere within my configuration.

;;; Code:

;; Useful string utilities, e.g. 's-contains-p'.
(use-package s :straight t)

(use-package dash :straight t)

(defun sm/run-and-bury (fn &rest args)
  "Run FN with ARGS then bury the buffer."
  (let ((buf (buffer-name)))
    (apply fn args)
    (bury-buffer buf)))

(defun sm/format-github-url (origin branch filepath beg &optional end)
  (let* ((repo
          (thread-last origin
            (s-chop-suffixes '("/" ".git"))
            (s-chop-prefixes '("git@github.com:" "https://github.com/"))))
         (url (format "https://github.com/%s/tree/%s/%s#L%d" repo branch filepath beg)))
    (if end
        (concat url (format "-L%d" end))
      url)))

(defun sm/browse-github-url-at-point (beg end)
  "Open file at point on github using BEG and END to link to the correct section of code."
  (interactive (if (use-region-p)
                   (list (line-number-at-pos (region-beginning))
                         (line-number-at-pos (region-end)))
                 (list (line-number-at-pos) nil)))
  (when (magit-toplevel)
    (let ((origin (magit-get "remote.origin.url"))
          (branch (magit-get-current-branch))
          (filepath (s-chop-prefix (magit-toplevel) (buffer-file-name))))
      (browse-url (sm/format-github-url origin branch filepath beg end)))))

(provide 'seanmacs-libs)
;;; seanmacs-libs.el ends here

