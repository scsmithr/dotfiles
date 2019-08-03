;; Hide some things.
;; Do this first so that I never see the menu bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(defmacro after! (target &rest body)
  "Load BODY after TARGET."
  `(with-eval-after-load ',target ,@body))

(defun face-attr (face &rest args)
  (apply #'set-face-attribute face nil args))

(defun set-evil-initial-state (modes state)
  "Set the initialize STATE of MODES using `evil-set-initial-state'."
  (declare (indent defun))
  (after! evil
    (if (listp modes)
        (dolist (mode modes)
          (evil-set-initial-state mode state))
      (evil-set-initial-state modes state))))

;; Core configuration

(use-package keybinds
  :load-path "lisp"
  :config
  (core/init-leader))

(use-package ui
  :load-path "lisp")

(use-package completions
  :load-path "lisp")

(use-package utils
  :load-path "lisp")

(use-package orgmode
  :load-path "lisp")

(use-package modeline
  :load-path "lisp"
  :config
  (modeline-mode))

(use-package langs
  :load-path "lisp")

(use-package email
  :load-path "lisp"
  :init
  (core/leader
   "a e" 'mu4e))

;; Useful functions

