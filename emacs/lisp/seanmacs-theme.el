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
          (face-attr 'whitespace-tab :background nil)
          (face-attr 'trailing-whitespace :background (doom-transparentize 'red 0.8)))

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
          (face-attr 'ripgrep-match-face :inherit 'highlight))

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
                     :weight 'normal
                     :foreground nil
                     :background (doom-color 'base3)))

  (after! diff-hl
          (face-attr 'diff-hl-insert
                     :foreground (doom-transparentize 'green 0.5)
                     :background (doom-transparentize 'green 0.5))
          (face-attr 'diff-hl-delete
                     :foreground (doom-transparentize 'red 0.5)
                     :background (doom-transparentize 'red 0.5))
          (face-attr 'diff-hl-change
                     :foreground (doom-transparentize 'yellow 0.5)
                     :background (doom-transparentize 'yellow 0.5)))

  (after! dired
          (face-attr 'dired-directory
                     :foreground (doom-color 'blue))
          (face-attr 'dired-header
                     :foreground (doom-color 'fg)))

  (after! magit
          (face-attr 'magit-header-line
                     :background nil
                     :foreground (doom-color 'fg)
                     :weight 'bold
                     :box nil))

  (after! lsp
          (face-attr 'lsp-face-highlight-textual
                     :weight 'normal
                     :foreground nil
                     :background (doom-color 'base3)))

  (after! tide
          (face-attr 'tide-hl-identifier-face
                     :weight 'normal
                     :foreground nil
                     :background (doom-color 'base3)))

  (face-attr 'highlight
             :weight 'bold
             :foreground nil
             :distant-foreground nil
             :background (doom-darken 'bg-alt 0.1))
  (face-attr 'lazy-highlight
             :weight 'bold
             :foreground nil
             :distant-foreground nil
             :background (doom-darken 'bg-alt 0.1))
  (face-attr 'fringe :background (doom-color 'bg))
  (face-attr 'font-lock-comment-face :foreground (doom-lighten 'fg 0.25))
  (face-attr 'font-lock-doc-face :foreground (doom-lighten 'fg 0.25))
  (face-attr 'font-lock-keyword-face :weight 'normal)
  (face-attr 'font-lock-constant-face :weight 'normal)
  (face-attr 'show-paren-match
             :weight 'bold
             :background (doom-transparentize 'cyan 0.5)
             :foreground (doom-color 'cyan)))

(use-package all-the-icons
  :straight t
  :config
  (setq all-the-icons-color-icons nil
        all-the-icons-scale-factor 1.0
        all-the-icons-default-adjust 0.0)
  (setq all-the-icons-icon-alist
        '(
          ("\\.md$" all-the-icons-octicon "file-text")
          ("\\.org$" all-the-icons-octicon "file-text")
          ("\\.txt$" all-the-icons-octicon "file-text")
          ("\\.log$" all-the-icons-octicon "file-text")
          ("\\.rst$" all-the-icons-octicon "file-text")
          ("\\.o$" all-the-icons-octicon "file-binary")
          ("\\.exe$" all-the-icons-octicon "file-binary")
          ("\\.so$" all-the-icons-octicon "file-binary")
          ("\\.out$" all-the-icons-octicon "file-binary")
          ("\\.pdf$" all-the-icons-octicon "file-pdf")
          ("\\.zip$" all-the-icons-octicon "file-zip")
          ("\\.tar$" all-the-icons-octicon "file-zip")
          ("\\.tgz$" all-the-icons-octicon "file-zip")
          ("\\.gz$" all-the-icons-octicon "file-zip")
          ("\\.jpg$" all-the-icons-octicon "file-media")
          ("\\.jpeg$" all-the-icons-octicon "file-media")
          ("\\.png$" all-the-icons-octicon "file-media")
          ("\\.gif$" all-the-icons-octicon "file-media")
          ("\\.svg$" all-the-icons-octicon "file-media")
          ("\\.mkv$" all-the-icons-octicon "file-media")
          ("\\.mp3$" all-the-icons-octicon "file-media")
          ("\\.mp4$" all-the-icons-octicon "file-media")
          ("\\.ogg$" all-the-icons-octicon "file-media")
          ("\\.midi$" all-the-icons-octicon "file-media")
          ("." all-the-icons-octicon "file-code")))
  (setq all-the-icons-dir-icon-alist
        '(
          ("." all-the-icons-octicon "file-directory"))))

(use-package all-the-icons-dired
  :straight t
  :config
  (setq all-the-icons-dired-v-adjust 0.0)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
