;;; seanmacs-theme.el --- Theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme stuff.

;;; Code:

(defun face-attr (face &rest args)
  (apply #'set-face-attribute face nil args))

(defun reset-face (face)
  (apply #'face-spec-reset-face face nil))

;; Set fonts.
(add-to-list 'default-frame-alist `(font . "Fira Mono-11"))
(face-attr 'variable-pitch :weight 'light :family "Fira Sans-11")

(defvar seanmacs/load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun seanmacs/run-load-theme-hook (&rest _)
  "Run after `seanmacs/load-theme-hook'."
  (run-hooks 'seanmacs/load-theme-hook))

(advice-add #'load-theme :after #'seanmacs/run-load-theme-hook)

(use-package modus-operandi-theme
  :straight t
  :config
  (setq modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-diffs 'desaturated
        modus-operandi-theme-section-headings nil
        modus-operandi-theme-scale-headings nil
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-org-blocks 'greyscale
        modus-operandi-theme-fringes 'subtle
        modus-operandi-theme-completions 'opinionated)

  (setq modus-operandi-theme-override-colors-alist
        '(("fg-main"                 . "#26272d")
          ("bg-main"                 . "#fcfcfc")
          ("fg-alt"                  . "#808080")
          ("bg-region"               . "#dfdfdf")
          ("fg-whitespace"           . "#dadada")
          ("bg-whitespace"           . "#fcfcfc")
          ("fg-window-divider-inner" . "#dadada")
          ("fg-window-divider-outer" . "#aaaaaa")))

  (defun seanmacs/customize-modus-operandi ()
    (modus-operandi-theme-with-color-variables
      (face-attr 'success :foreground green :weight 'normal)
      (face-attr 'warning :foreground yellow :weight 'normal)
      (face-attr 'error :foreground red :weight 'normal)
      (face-attr 'region :background bg-region :foreground 'unspecified)

      (custom-theme-set-faces
       'modus-operandi
       ;; Whitespace
       `(whitespace-hspace ((t (:foreground ,fg-whitespace))))
       `(whitespace-indentation ((t (:foreground ,fg-whitespace))))
       `(whitespace-line ((t (:background ,bg-dim))))
       `(whitespace-newline ((t (:foreground ,fg-whitespace))))
       `(whitespace-space ((t (:foreground ,fg-whitespace))))
       `(whitespace-tab ((t (:foreground ,fg-whitespace))))
       ;; Fringe
       `(fringe ((t (,@(modus-operandi-theme-fringe bg-inactive bg-active)
                     :foreground ,fg-alt))))
       ;; Modeline
       `(mode-line ((t (
                        :background ,bg-active
                        :foreground ,fg-active
                        :box ,(modus-operandi-theme-modeline-box bg-active fg-window-divider-outer t)))))
       `(mode-line-inactive ((t (
                                 :background ,bg-inactive
                                 :foreground ,fg-inactive
                                 :box ,(modus-operandi-theme-modeline-box bg-inactive fg-window-divider-inner t)))))
       ;; Extend highlights
       `(mu4e-header-highlight-face ((t (:inherit modus-theme-hl-line :extend t))))
       `(flycheck-error-list-highlight ((t (:inherit modus-theme-hl-line :extend t))))
       ;; Company (mostly matches opinionated completions theming)
       `(company-preview ((t (:inherit (modus-theme-nuanced-magenta) :foreground ,magenta))))
       `(company-preview-common ((t (:inherit (modus-theme-nuanced-blue) :foreground ,blue))))
       ;; Eshell
       `(eshell-ls-directory ((t (:foreground ,blue-alt)))))))

  (add-hook 'seanmacs/load-theme-hook 'seanmacs/customize-modus-operandi)
  (load-theme 'modus-operandi t))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
