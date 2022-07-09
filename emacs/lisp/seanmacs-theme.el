;;; seanmacs-theme.el --- Theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme stuff.

;;; Code:

(eval-when-compile
  (require 'use-package))

(defun sm/setup-frame-fonts ()
  "Setup fonts for currently active frame."
  (when (display-graphic-p)
    (message "Setting frame fonts")
    (set-face-attribute 'default nil :family "PragmataPro Mono" :height 120 :weight 'normal)
    (set-face-attribute 'variable-pitch nil :family "Source Serif Pro" :height 130)
    ;; Unicode fallbacks.
    (set-fontset-font t 'unicode (font-spec :name "PragmataPro Mono" :weight 'normal))
    (set-fontset-font t 'unicode (font-spec :name "DejaVu Sans" :weight 'normal) nil 'append)))

;; Set default font. When not running in a daemon, this will ensure the frame
;; has the appropriate font set.
(add-to-list 'default-frame-alist '(font . "PragmataPro Mono"))

;; If running as a daemon, make sure fonts are set everytime a new frame is
;; created. This ensures unicode fallbacks are set for all frames.
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'sm/setup-frame-fonts)
  (sm/setup-frame-fonts))

(blink-cursor-mode -1)

(setq mode-line-compact 'long)

(use-package minions
  :straight t
  :config
  ;; Flymake - I always want to see warnings/errors.
  ;; Follow - Can slow things down quite a bit, so nice to know if it's enabled.
  (setq minions-direct '(flymake-mode follow-mode)
        minions-mode-line-lighter "…")

  (minions-mode 1))

(defvar sm/load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun sm/run-load-theme-hook (&rest _)
  "Run after `sm/load-theme-hook'."
  (run-hooks 'sm/load-theme-hook))

(advice-add 'load-theme :after #'sm/run-load-theme-hook)

(use-package modus-themes
  :straight t
  :config
  (fringe-mode '(16 . 16))

  (setq modus-themes-fringes 'subtle
        modus-themes-org-blocks 'gray-background
        modus-themes-region '(bg-only)
        modus-themes-diffs 'bg-only
        modus-themes-italic-constructs t
        modus-themes-lang-checkers '(straight-underline)
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
       ;; Org mode
       `(org-code ((t (:inherit modus-themes-markup-verbatim))))
       ;; Modus fringes
       `(modus-themes-fringe-red ((t :foreground ,red-fringe-bg)))
       `(modus-themes-fringe-green ((t :foreground ,green-fringe-bg)))
       `(modus-themes-fringe-yellow ((t :foreground ,yellow-fringe-bg)))
       `(modus-themes-fringe-blue ((t :foreground ,blue-fringe-bg)))
       `(modus-themes-fringe-magenta ((t :foreground ,magenta-fringe-bg)))
       `(modus-themes-fringe-cyan ((t :foreground ,cyan-fringe-bg)))
       ;; Eldoc
       `(eldoc-highlight-function-argument ((t :inherit bold :foreground ,blue-alt-other)))

       (with-eval-after-load 'eglot
         (set-face-attribute 'eglot-highlight-symbol-face nil :background cyan-nuanced-bg :weight 'normal)
         (set-face-attribute 'eglot-diagnostic-tag-deprecated-face nil :inherit 'unspecified)
         (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil :inherit 'unspecified)))))

  (add-hook 'sm/load-theme-hook 'sm/customize-modus-operandi)
  (load-theme 'modus-operandi t))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
