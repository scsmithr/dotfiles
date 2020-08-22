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

(defvar seanmacs/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun seanmacs/run-after-load-theme-hook (&rest _)
  "Run after `seanmacs/after-load-theme-hook'."
  (run-hooks 'seanmacs/after-load-theme-hook))

(advice-add #'load-theme :after #'seanmacs/run-after-load-theme-hook)

(use-package modus-operandi-theme
  :straight t
  :config

  (setq modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-section-headings t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-org-blocks 'greyscale
        modus-operandi-theme-fringes 'subtle
        modus-operandi-theme-completions 'opinionated)

  (setq modus-operandi-theme-override-colors-alist
        '(("fg-main" . "#26272d")
          ("bg-main" . "#fcfcfc")
          ("bg-region" . "#dfdfdf")
          ("fg-whitespace" . "#dadada")
          ("bg-whitespace" . "#fcfcfc")))

  (defun seanmacs/customize-modus-operandi ()
    (modus-operandi-theme-with-color-variables
      (face-attr 'success :foreground green :weight 'normal)
      (face-attr 'warning :foreground yellow :weight 'normal)
      (face-attr 'error :foreground red :weight 'normal)
      (face-attr 'region :background bg-region :foreground 'unspecified)

      (custom-theme-set-faces
       'modus-operandi
       `(whitespace-hspace ((t (:foreground ,fg-whitespace))))
       `(whitespace-indentation ((t (:foreground ,fg-whitespace))))
       `(whitespace-line ((t (:background ,bg-dim))))
       `(whitespace-newline ((t (:foreground ,fg-whitespace))))
       `(whitespace-space ((t (:foreground ,fg-whitespace))))
       `(whitespace-tab ((t (:foreground ,fg-whitespace))))
       `(flycheck-error-list-highlight ((t (:background ,bg-hl-line :extend t))))
       `(eshell-ls-directory ((t (:foreground ,blue-alt)))))))

  (add-hook 'seanmacs/after-load-theme-hook 'seanmacs/customize-modus-operandi)
  (load-theme 'modus-operandi t))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
