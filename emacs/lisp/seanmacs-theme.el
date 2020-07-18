;;; seanmacs-theme.el --- Theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme stuff.

;;; Code:

(defun face-attr (face &rest args)
  (apply #'set-face-attribute face nil args))

(defun reset-face (face)
  (apply #'face-spec-reset-face face nil))

;; Default font
(let ((default-monospace "Fira Mono"))
  (set-frame-font (font-spec :family default-monospace))
  (face-attr 'default :font default-monospace :height 110))
(face-attr 'variable-pitch :weight 'light :family "Fira Sans" :height 110)

(use-package modus-operandi-theme
  :straight (modus-operandi-theme :type git :host github :repo "scsmithr/modus-themes")
  :config
  (setq modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-rainbow-headings t)
  (load-theme 'modus-operandi t))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
