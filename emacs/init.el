;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration.

;;; Code:

;; Hide some things.
;; Do this first so that I never see the menu bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; GC things
(setq gc-cons-threshold 20000000)

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

(defun set-evil-initial-state (modes state)
  "Set the initialize STATE of MODES using `evil-set-initial-state'."
  (declare (indent defun))
  (after! evil
          (if (listp modes)
              (dolist (mode modes)
                (evil-set-initial-state mode state))
            (evil-set-initial-state modes state))))

(defun seanmacs/shackle (rules)
  "Display buffers using RULES."
  (dolist (rule rules)
    (let* ((condition (car rule))
           (plist (cdr rule))
           (action (or (plist-get plist :action) #'seanmacs/display-buffer-below))
           (height (or (plist-get plist :height) #'fit-window-to-buffer))
           (alist `(window-height . ,height)))
      (if (symbolp condition)
          (add-to-list
           'display-buffer-alist
           `((lambda (buffer &optional action)
               (eq (quote ,condition)
                   (buffer-local-value 'major-mode (get-buffer buffer))))
             ,action ,alist))
        (add-to-list 'display-buffer-alist `(,condition ,action ,alist))))))

(defalias 'shackle 'seanmacs/shackle)

(defun seanmacs/display-buffer-below (buffer alist)
  "Display BUFFER below current, using ALIST."
  (when-let (window (display-buffer-below-selected buffer alist))
    (select-window window)))

(defun seanmacs/display-buffer-bottom (buffer alist)
  "Display BUFFER at the bottom, using ALIST."
  (when-let (window (display-buffer-at-bottom buffer alist))
    (select-window window)))

(defun seanmacs/display-buffer-same (buffer alist)
  (display-buffer-same-window buffer alist))

(use-package seanmacs-keybinds
  :load-path "lisp"
  :config
  (core/init-leader))

(use-package seanmacs-funcs
  :load-path "lisp")

(use-package seanmacs-theme
  :load-path "lisp")

(use-package seanmacs-windows
  :load-path "lisp")

(use-package seanmacs-edit
  :load-path "lisp")

(use-package seanmacs-completions
  :load-path "lisp")

(use-package seanmacs-utils
  :load-path "lisp")

(use-package seanmacs-version-control
  :load-path "lisp")

(use-package seanmacs-org
  :load-path "lisp")

(use-package seanmacs-modeline
  :load-path "lisp"
  :config
  (modeline-mode))

(use-package seanmacs-langs
  :load-path "lisp")

(use-package seanmacs-shell
  :load-path "lisp")

(use-package seanmacs-email
  :load-path "lisp")

;;; init.el ends here
