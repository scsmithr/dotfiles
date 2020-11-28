;;; seanmacs-libs.el --- Functions/libraries -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions and libraries that should be loaded early on, and can be used
;; elsewhere within my configuration.

;;; Code:

;; Useful string utilities, e.g. 's-contains-p'.
(use-package s :straight t)

(defun sm/run-and-bury (fn &rest args)
  "Run FN with ARGS then bury the buffer."
  (let ((buf (buffer-name)))
    (apply fn args)
    (bury-buffer buf)))

(provide 'seanmacs-libs)
;;; seanmacs-libs.el ends here

