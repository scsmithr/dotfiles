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

(setq initial-scratch-message ";; Not Invented Here\n\n")

;; GC things
(setq gc-cons-threshold 20000000)
(setq read-process-output-max (* 1024 1024))

;; Get straight.el
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
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

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(use-package seanmacs-libs)
(use-package seanmacs-theme)
(use-package seanmacs-keybinds)
(use-package seanmacs-windows)
(use-package seanmacs-edit)
(use-package seanmacs-completions)
(use-package seanmacs-utils)
(use-package seanmacs-version-control)
(use-package seanmacs-org)
(use-package seanmacs-langs)
(use-package seanmacs-shell)
(use-package seanmacs-email)
(use-package seanmacs-password)

;;; init.el ends here
