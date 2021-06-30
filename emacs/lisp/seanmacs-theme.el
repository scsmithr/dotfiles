;;; seanmacs-theme.el --- Theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme stuff.

;;; Code:

(defun sm/setup-fonts (&optional frame)
  "Setup fonts for FRAME."
  (when (and frame (display-graphic-p frame))
    (message "Setting fonts")
    (set-frame-font (font-spec :name "Fira Mono") t t)
    (set-face-attribute 'default nil :family "Fira Mono" :height 110 :weight 'normal)
    (set-face-attribute 'variable-pitch nil :family "Source Serif Pro" :height 130 :weight 'normal)
    ;; Unicode fallbacks.
    (set-fontset-font t 'unicode (font-spec :name "Fira Mono"))
    (set-fontset-font t 'unicode (font-spec :name "Fira Code") nil 'append)
    (set-fontset-font t 'unicode (font-spec :name "JuliaMono" :weight 'light) nil 'append)
    (set-fontset-font t 'unicode (font-spec :name "DejaVu Sans") nil 'append)))

;; Set fonts.
(if (display-graphic-p)
    (sm/setup-fonts (selected-frame))
  (add-hook 'after-make-frame-functions #'sm/setup-fonts))

(use-package minions
  :straight t
  :config
  ;; Flycheck - I always want to see warnings/errors.
  ;; Follow - Can slow things down quite a bit, so nice to know if it's enabled.
  (setq minions-direct '(flycheck-mode follow-mode))

  (minions-mode 1))

(defvar sm/load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun sm/run-load-theme-hook (&rest _)
  "Run after `sm/load-theme-hook'."
  (run-hooks 'sm/load-theme-hook))

(advice-add #'load-theme :after #'sm/run-load-theme-hook)

(use-package modus-themes
  :straight t
  :config
  (fringe-mode '(10 . 10))

  (setq modus-themes-fringes 'subtle
        modus-themes-org-blocks 'gray-background
        modus-themes-region 'bg-only
        modus-themes-diffs 'bg-only)

  (setq x-underline-at-descent-line t)

  (setq modus-themes-operandi-color-overrides
        '((fg-whitespace           . "#dadada")
          (bg-whitespace           . "#fcfcfc")
          (fg-window-divider-inner . "#dadada")
          (fg-window-divider-outer . "#aaaaaa")))

  (defun sm/customize-modus-operandi ()
    (modus-themes-with-colors
      (custom-theme-set-faces
       'modus-operandi
       ;; Whitespace
       `(whitespace-hspace ((t (:foreground ,fg-whitespace))))
       `(whitespace-indentation ((t (:foreground ,fg-whitespace))))
       `(whitespace-line ((t (:background ,bg-dim))))
       `(whitespace-newline ((t (:foreground ,fg-whitespace))))
       `(whitespace-space ((t (:foreground ,fg-whitespace))))
       `(whitespace-tab ((t (:foreground ,fg-whitespace))))
       ;; Company
       `(company-preview ((t (:background ,bg-hl-line :foreground ,fg-alt))))
       `(company-preview-common ((t (:background ,bg-hl-line :foreground ,blue))))
       ;; Eshell
       `(eshell-ls-directory ((t (:foreground ,blue-alt)))))

      (with-eval-after-load 'sly-mrepl
        (set-face-attribute 'sly-mrepl-output-face nil :foreground cyan))))

  (add-hook 'sm/load-theme-hook 'sm/customize-modus-operandi)
  (load-theme 'modus-operandi t))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
