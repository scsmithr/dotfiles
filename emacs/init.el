;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration.

;;; Code:

(setq initial-scratch-message ";; Not Invented Here\n\n")

;; Suppress native comp warnings (for now).
(setq warning-minimum-level :error)

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

(setq use-package-enable-imenu-support t)

;; Ensure use-package is here.
(straight-use-package 'use-package)

;; Ensure development versions of built in packages are registered.
(straight-use-package 'org)
(straight-use-package 'eldoc)
(straight-use-package 'project)
(straight-use-package 'xref)
(straight-use-package 'flymake)
(straight-use-package 'transient)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'seanmacs-libs)
(require 'seanmacs-theme)
(require 'seanmacs-evil)
(require 'seanmacs-windows)
(require 'seanmacs-edit)
(require 'seanmacs-completions)
(require 'seanmacs-flymake)
(require 'seanmacs-utils)
(require 'seanmacs-elfeed)
(require 'seanmacs-version-control)
(require 'seanmacs-org)
(require 'seanmacs-langs)
(require 'seanmacs-shell)
(require 'seanmacs-email)
(require 'seanmacs-password)

;;; init.el ends here

