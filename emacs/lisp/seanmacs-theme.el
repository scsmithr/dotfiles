;;; seanmacs-theme.el --- Theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme stuff.

;;; Code:

(defun sm/setup-fonts (&optional frame)
  "Setup fonts for FRAME."
  (message "Setting fonts")
  (set-face-attribute 'default nil :family "Triplicate A" :height 120 :weight 'normal)
  (set-face-attribute 'variable-pitch nil :family "Source Serif Pro" :height 120)
  ;; Unicode fallbacks.
  (set-fontset-font t 'unicode (font-spec :name "Triplicate A" :weight 'normal))
  (set-fontset-font t 'unicode (font-spec :name "JuliaMono" :weight 'light) nil 'append)
  (set-fontset-font t 'unicode (font-spec :name "DejaVu Sans" :weight 'normal) nil 'append))

(sm/setup-fonts)
;; Setup fonts on every frame load so that the unicode fontsets get set
;; everytime.
(add-hook 'after-make-frame-functions #'sm/setup-fonts)

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
  (fringe-mode '(16 . 16))

  (setq modus-themes-fringes 'subtle
        modus-themes-org-blocks 'gray-background
        modus-themes-region 'bg-only
        modus-themes-diffs 'bg-only
        modus-themes-org-agenda '((header-block . (no-scale))
                                  (header-date . (bold-all underline-today))))

  (setq x-underline-at-descent-line t)

  (setq modus-themes-operandi-color-overrides
        '((fg-whitespace           . "#f2eff3")
          (bg-whitespace           . "#ffffff")
          (fg-window-divider-inner . "#dadada")
          (fg-window-divider-outer . "#aaaaaa")
          ;; Custom fringe colors, I'm using these 'bg' colors as the
          ;; foreground.
          (red-fringe-bg     . "#c06873")
          (green-fringe-bg   . "#4ea054")
          (yellow-fringe-bg  . "#af9432")
          (blue-fringe-bg    . "#688ccc")
          (magenta-fringe-bg . "#b382cc")
          (cyan-fringe-bg    . "#25a4b2")))

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
       ;; Org mode
       `(org-code ((t (:inherit modus-themes-fixed-pitch :foreground ,magenta-nuanced-fg :background ,magenta-nuanced-bg))))
       ;; Modus fringes
       `(modus-themes-fringe-red ((t :foreground ,red-fringe-bg)))
       `(modus-themes-fringe-green ((t :foreground ,green-fringe-bg)))
       `(modus-themes-fringe-yellow ((t :foreground ,yellow-fringe-bg)))
       `(modus-themes-fringe-blue ((t :foreground ,blue-fringe-bg)))
       `(modus-themes-fringe-magenta ((t :foreground ,magenta-fringe-bg)))
       `(modus-themes-fringe-cyan ((t :foreground ,cyan-fringe-bg)))

       (with-eval-after-load 'sly-mrepl
         (set-face-attribute 'sly-mrepl-output-face nil :foreground cyan))

       (with-eval-after-load 'eglot
         (set-face-attribute 'eglot-highlight-symbol-face nil :background cyan-nuanced-bg :weight 'normal)))))

  (add-hook 'sm/load-theme-hook 'sm/customize-modus-operandi)
  (load-theme 'modus-operandi t))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
