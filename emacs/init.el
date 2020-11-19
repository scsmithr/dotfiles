;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration.

;;; Code:

;; Hide some things.
;; Do this first so that I never see the menu bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-x-resources t
      inhibit-startup-message t
      use-dialog-box nil
      inihibit-startup-echo-area-message t)

;; GC things
(setq gc-cons-threshold 20000000)
(setq read-process-output-max (* 1024 1024))

;; Get straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Ensure use-package is here.
(straight-use-package 'use-package)

(defun seanmacs/run-and-bury (fn &rest args)
  "Run FN with ARGS then bury the buffer."
  (let ((buf (buffer-name)))
    (apply fn args)
    (bury-buffer buf)))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(use-package seanmacs-libs)
(use-package seanmacs-theme)
(use-package seanmacs-keybinds
  :config
  (seanmacs/init-leader))
(use-package seanmacs-windows)
(use-package seanmacs-edit)
(use-package seanmacs-completions)
(use-package seanmacs-utils)
(use-package seanmacs-version-control)
(use-package seanmacs-org)
(use-package seanmacs-langs)
(use-package seanmacs-shell)
(use-package seanmacs-email)

;;; init.el ends here
