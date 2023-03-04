;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration.

;;; Code:

;; See https://github.com/jrblevin/markdown-mode/issues/578
(defvar native-comp-jit-compilation-deny-list nil)
(add-to-list 'native-comp-jit-compilation-deny-list "markdown-mode\\.el$")

(setq initial-scratch-message ";; Not Invented Here\n\n")

;; Suppress native comp warnings (for now).
(setq native-comp-async-report-warnings-errors :silent
      warning-suppress-log-types '((comp)))

;; GC things
(setq gc-cons-threshold 20000000)
(setq read-process-output-max (* 1024 1024))

;; Make sure there's no gaps around the window when full screening
(setq frame-resize-pixelwise t)

;; Get straight.el
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(straight-use-package '(org :type built-in))
(straight-use-package '(eglot :type built-in))
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))

(bind-keys :prefix "C-c g" :prefix-map git-prefix-map)
(bind-keys :prefix "C-c a" :prefix-map app-prefix-map)
(bind-keys :prefix "C-c s" :prefix-map shell-prefix-map)
(bind-keys :prefix "C-c b" :prefix-map buffer-prefix-map)

(require 'seanmacs-libs)
(require 'seanmacs-straight)
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
(require 'seanmacs-password)

;;; init.el ends here

