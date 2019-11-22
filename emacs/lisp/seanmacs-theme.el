;;; seanmacs-theme.el --- Theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme stuff.

;;; Code:

(defun face-attr (face &rest args)
  (apply #'set-face-attribute face nil args))

;; Default font
(set-frame-font (font-spec :family "Source Code Pro"))
(face-attr 'default :height 110)
(face-attr 'variable-pitch :family "Source Sans Pro" :height 120)

(defun doom-transparentize (color alpha)
  "Transparentize a COLOR (a hexidecimal string) by a coefficient
ALPHA (a float between 0 and 1)."
  (doom-blend (doom-color color) (doom-color 'bg) (- 1 alpha)))

;; Doom themes
(use-package doom-themes
  :straight t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-solarized-light t)

  (after! whitespace
          (face-attr 'whitespace-line
                     :weight 'normal
                     :foreground 'unspecified)
          (face-attr 'whitespace-tab :background nil))

  (after! org
          (face-attr 'org-meta-line :foreground (doom-color 'fg-alt))
          (face-attr 'org-block-begin-line
                     :foreground (doom-lighten 'fg-alt 0.2)
                     :background (doom-blend 'yellow 'bg 0.04))
          (face-attr 'org-block-end-line
                     :foreground (doom-lighten 'fg-alt 0.2)
                     :background (doom-blend 'yellow 'bg 0.04))
          (face-attr 'org-done :foreground (doom-color 'green))
          (face-attr 'org-todo :foreground (doom-color 'yellow)))

  (after! markdown-mode
          (face-attr markdown-pre-face :background (doom-blend 'yellow 'bg 0.04))
          (face-attr 'markdown-code-face :background (doom-blend 'yellow 'bg 0.04))
          (face-attr markdown-header-delimiter-face :foreground (doom-color 'fg-alt))
          (face-attr markdown-header-face-1 :inherit 'outline-1)
          (face-attr markdown-header-face-2 :inherit 'outline-2)
          (face-attr markdown-header-face-3 :inherit 'outline-3)
          (face-attr markdown-header-face-4 :inherit 'outline-4)
          (face-attr markdown-header-face-5 :inherit 'outline-5)
          (face-attr markdown-header-face-6 :inherit 'outline-6))

  (after! ripgrep
          (face-attr 'ripgrep-match-face :foreground (doom-color 'yellow)))

  (after! flycheck
          (face-attr 'flycheck-fringe-info
                     :foreground (doom-transparentize 'green 0.5)
                     :background (doom-transparentize 'green 0.5))
          (face-attr 'flycheck-fringe-warning
                     :foreground (doom-transparentize 'orange 0.5)
                     :background (doom-transparentize 'orange 0.5))
          (face-attr 'flycheck-fringe-error
                     :foreground (doom-transparentize 'red 0.5)
                     :background (doom-transparentize 'red 0.5)))

  (after! flx-ido
          (face-attr 'flx-highlight-face :foreground (doom-color 'magenta)))

  (after! mu4e
          (face-attr 'mu4e-highlight-face
                     :background (doom-color 'bg)
                     :foreground (doom-color 'blue))
          (face-attr 'mu4e-header-highlight-face
                     :inherit 'hl-line
                     :underline nil))

  (after! web-mode
          (face-attr 'web-mode-current-element-highlight-face
                     :weight 'bold
                     :background (doom-transparentize 'cyan 0.5)))

  (after! elixir-mode
          ;; Defaults to dark blue with doom emacs theme. Doom solarized light seems
          ;; to have it set to some default color, isn't easy to read.
          (face-attr 'elixir-atom-face :foreground (doom-color 'blue)))

  (after! diff-hl
          (face-attr 'diff-hl-insert
                     :foreground (doom-color 'green)
                     :background (doom-color 'green))
          (face-attr 'diff-hl-delete
                     :foreground (doom-color 'red)
                     :background (doom-color 'red))
          (face-attr 'diff-hl-change
                     :foreground (doom-color 'yellow)
                     :background (doom-color 'yellow)))

  (after! dired
          (face-attr 'dired-directory
                     :foreground (doom-color 'blue))
          (face-attr 'dired-header
                     :foreground (doom-color 'fg)))

  (face-attr 'fringe :background (doom-color 'bg))
  (face-attr 'font-lock-comment-face :foreground (doom-lighten 'fg 0.25))
  (face-attr 'font-lock-doc-face :foreground (doom-lighten 'fg 0.25))
  (face-attr 'show-paren-match
             :weight 'bold
             :background (doom-transparentize 'cyan 0.5)
             :foreground (doom-color 'cyan))
  (face-attr 'line-number :foreground (doom-color 'fg-alt)))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
