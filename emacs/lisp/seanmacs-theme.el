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
    (set-face-attribute 'default nil :family "PragmataPro Mono" :height 140 :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :family "PragmataPro Mono" :height 140 :weight 'normal)
    (set-face-attribute 'variable-pitch nil :family "Source Serif 4" :height 150)
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
  (add-hook 'window-setup-hook #'sm/setup-frame-fonts))

(setq frame-title-format "emacs")

(blink-cursor-mode -1)

(setq mode-line-compact nil)
(setq column-number-mode t)

(use-package minions
  :straight t
  :config
  ;; Flymake - I always want to see warnings/errors.
  ;; Follow - Can slow things down quite a bit, so nice to know if it's enabled.
  (setq minions-direct '(flymake-mode follow-mode)
        minions-mode-line-lighter "…")

  (minions-mode 1))

(use-package modus-themes
  :straight t
  :config
  (fringe-mode '(8 . 8))

  (setq modus-themes-fringes 'subtle
        modus-themes-org-blocks 'gray-background
        modus-themes-region '(bg-only)
        modus-themes-links '(faint)
        modus-themes-hl-line nil
        modus-themes-completions nil
        modus-themes-diffs 'bg-only
        modus-themes-italic-constructs nil
        modus-themes-lang-checkers '(straight-underline)
        modus-themes-org-agenda '((header-block . (no-scale))
                                  (header-date . (bold-all underline-today))))

  (setq x-underline-at-descent-line t)

  (setq modus-themes-operandi-color-overrides
        '((fg-whitespace           . "#f2eff3")
          (bg-whitespace           . "#ffffff")
          (fg-window-divider-inner . "#dadada")
          (fg-window-divider-outer . "#aaaaaa")
          ;; Tweaked doc/alt colors. Lightened to have docs and comments stand
          ;; out a bit more from surrounding text.
          (fg-docstring . "#6a6a90")
          (fg-alt       . "#707070")
          ;; Custom fringe colors, I'm using these 'bg' colors as the
          ;; foreground.
          (red-fringe-bg     . "#c06873")
          (green-fringe-bg   . "#4ea054")
          (yellow-fringe-bg  . "#af9432")
          (blue-fringe-bg    . "#688ccc")
          (magenta-fringe-bg . "#b382cc")
          (cyan-fringe-bg    . "#25a4b2")))

  (setq modus-themes-vivendi-color-overrides
        '((bg-main . "#101010") (fg-main . "#f9f9f9")
          (bg-dim . "#141314")
          (bg-hl-line . "#1a1d28")
          (fg-whitespace . "#1f222d")
          (bg-whitespace . "#101010")
          (fg-window-divider-inner . "#444444")
          (fg-window-divider-outer . "#646464")))

  (defun sm/customize-modus ()
    (modus-themes-with-colors
      (custom-set-faces
       ;; Region (for unspecifying distant foreground).
       `(region ((,class :distant-foreground unspecified
                         ,@(modus-themes--region bg-region fg-main
                                                 bg-hl-alt-intense bg-region-accent
                                                 bg-region-accent-subtle))))
       ;; Whitespace
       `(whitespace-hspace ((t (:foreground ,fg-whitespace :background unspecified))))
       `(whitespace-indentation ((t (:foreground ,fg-whitespace :background unspecified))))
       `(whitespace-line ((t (:background ,bg-dim :inherit unspecified))))
       `(whitespace-newline ((t (:foreground ,fg-whitespace :background unspecified))))
       `(whitespace-space ((t (:foreground ,fg-whitespace :background unspecified))))
       `(whitespace-tab ((t (:foreground ,fg-whitespace :background unspecified))))
       ;; Org mode
       `(org-code ((t (:inherit modus-themes-markup-verbatim))))
       ;; Modus fringes
       `(modus-themes-fringe-red ((t :foreground ,red-fringe-bg :background unspecified)))
       `(modus-themes-fringe-green ((t :foreground ,green-fringe-bg :background unspecified)))
       `(modus-themes-fringe-yellow ((t :foreground ,yellow-fringe-bg :background unspecified)))
       `(modus-themes-fringe-blue ((t :foreground ,blue-fringe-bg :background unspecified)))
       `(modus-themes-fringe-magenta ((t :foreground ,magenta-fringe-bg :background unspecified)))
       `(modus-themes-fringe-cyan ((t :foreground ,cyan-fringe-bg :background unspecified)))
       ;; Modus reset
       `(modus-themes-reset-hard ((t :inherit default)))
       ;; Man/woman
       `(Man-overstrike ((t :inherit bold :foreground ,fg-special-calm)))
       `(woman-bold ((t :inherit bold :foreground ,fg-special-calm)))
       ;; Evil
       `(evil-ex-substitute-replacement ((t :inherit modus-themes-refine-green :underline t)))
       ;; Corfu
       `(corfu-default ((t :background ,bg-inactive)))
       ;; mode-line
       `(mode-line ((t :inherit modus-themes-ui-variable-pitch
                       ,@(modus-themes--mode-line-attrs
                          fg-active bg-active
                          fg-dim bg-active
                          fg-main bg-active-accent
                          bg-region bg-active
                          'alt-style bg-main))))
       ;; eglot
       `(eglot-highlight-symbol-face ((t :foreground ,yellow-alt-other :background ,yellow-nuanced-bg :weight normal)))
       `(eglot-diagnostic-tag-deprecated-face ((t :inherit unspecified)))
       `(eglot-diagnostic-tag-unnecessary-face ((t :inherit unspecified))))))

  (add-hook 'modus-themes-after-load-theme-hook 'sm/customize-modus)

  (load-theme 'modus-operandi t t)
  (load-theme 'modus-vivendi t t)
  (modus-themes-load-operandi)

  (defun sm/get-system-appearance ()
    "Get the system appearance.

Defaults to light if running in terminal or not running on mac."
    (if (and (display-graphic-p)
             (eq system-type 'darwin))
        (ns-do-applescript "
        tell application \"System Events\"
          tell appearance preferences
            if (dark mode) then
              return \"dark\"
            else
              return \"light\"
            end if
          end tell
        end tell
        ")
      "light"))

  (defun sm/toggle-system-appearance ()
    "Toggle system appearance if running on mac."
    (if (eq system-type 'darwin)
        (ns-do-applescript "
        tell application \"System Events\"
          tell appearance preferences
            set dark mode to not dark mode
          end tell
        end tell")))

  (defun sm/toggle-theme ()
    "Toggle the system theme along with the emacs theme."
    (interactive)
    (sm/toggle-system-appearance)
    (sm/sync-theme))

  (defun sm/sync-theme ()
    "Sync emacs theme with the system theme."
    (interactive)
    (if (string= (sm/get-system-appearance) "dark")
        (modus-themes-load-vivendi)
      (modus-themes-load-operandi)))

  ;; pdf-tools specific settings

  (defun sm/pdf-tools-backdrop ()
    (face-remap-add-relative
     'default
     `(:background ,(modus-themes-color 'bg-alt))))

  (defun sm/pdf-tools-midnight-mode-toggle ()
    (when (derived-mode-p 'pdf-view-mode)
      (if (eq (car custom-enabled-themes) 'modus-vivendi)
          (pdf-view-midnight-minor-mode 1)
        (pdf-view-midnight-minor-mode -1))
      (sm/pdf-tools-backdrop)))

  (defun sm/pdf-tools-themes-toggle ()
    (mapc
     (lambda (buf)
       (with-current-buffer buf
         (sm/pdf-tools-midnight-mode-toggle)))
     (buffer-list)))

  (add-hook 'pdf-tools-enabled-hook #'sm/pdf-tools-midnight-mode-toggle) ;; TODO: Get this hook working.
  (add-hook 'modus-themes-after-load-theme-hook #'sm/pdf-tools-themes-toggle))

;; Fringe bitmaps

(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b01111000
   #b01111000
   #b00011000
   #b00011000
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00011000
   #b00011000
   #b00011110
   #b00011110
   #b00000000
   #b00000000
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'right-arrow
  [#b00000000
   #b00100000
   #b00110000
   #b00111000
   #b00111100
   #b00111000
   #b00110000
   #b00100000
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'left-arrow
  [#b00000000
   #b00000100
   #b00001100
   #b00011100
   #b00111100
   #b00011100
   #b00001100
   #b00000100
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'up-arrow
  [#b00000000
   #b01111100
   #b00111100
   #b00011100
   #b00001100
   #b00000100
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'down-arrow
  [#b00000000
   #b00000100
   #b00001100
   #b00011100
   #b00111100
   #b01111100
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'right-triangle
  [#b00000000
   #b00100000
   #b00110000
   #b00111000
   #b00111100
   #b00111000
   #b00110000
   #b00100000
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'left-triangle
  [#b00000000
   #b00000100
   #b00001100
   #b00011100
   #b00111100
   #b00011100
   #b00001100
   #b00000100
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'sm/right-line-bmp [#b00011000] nil nil '(center t))
(define-fringe-bitmap 'sm/left-line-bmp [#b00011000] nil nil '(center t))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
