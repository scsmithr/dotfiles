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
    (set-face-attribute 'default nil :family "Iosevka Custom" :height 140 :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka Custom")
    (set-face-attribute 'variable-pitch nil :family "Source Serif 4" :height 140)
    ;; Unicode fallbacks.
    (set-fontset-font t 'unicode (font-spec :name "Iosevka Custom" :weight 'normal))
    (set-fontset-font t 'unicode (font-spec :name "DejaVu Sans" :weight 'normal) nil 'append)
    (when (eq system-type 'darwin)
      (set-fontset-font t 'unicode (font-spec :name "Apple Color Emoji" :size 10 :weight 'normal) nil 'append))))

;; Set default font. When not running in a daemon, this will ensure the frame
;; has the appropriate font set.
(add-to-list 'default-frame-alist '(font . "Iosevka Custom"))

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
  (setq minions-prominent-modes '(flymake-mode follow-mode)
        minions-mode-line-lighter "…")

  (minions-mode 1))

(use-package modus-themes
  :straight (:host sourcehut :repo "protesilaos/modus-themes" :branch "main")
  :config

  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-org-blocks 'gray-background
        modus-themes-prompts '(italic))

  (setq modus-themes-common-palette-overrides
        '(
          (fg-region unspecified)
          (bg-region bg-ochre)
          ))

  (defun sm/customize-modus ()
    (modus-themes-with-colors
      (custom-set-faces
       ;; Whitespace
       `(whitespace-hspace ((t (:foreground ,bg-dim :background unspecified))))
       `(whitespace-indentation ((t (:foreground ,bg-dim :background unspecified))))
       `(whitespace-line ((t (:background ,bg-dim :inherit unspecified))))
       `(whitespace-newline ((t (:foreground ,bg-dim :background unspecified))))
       `(whitespace-space ((t (:foreground ,bg-dim :background unspecified))))
       `(whitespace-tab ((t (:foreground ,bg-dim :background unspecified))))
       ;; Region (unspecify distant foreground)
       `(region ((t :distant-foreground unspecified :background ,bg-region :foreground ,fg-region)))
       ;; Eglot
       `(eglot-highlight-symbol-face ((t :background ,bg-hover-secondary :weight unspecified)))
       `(eglot-diagnostic-tag-deprecated-face ((t :inherit unspecified)))
       `(eglot-diagnostic-tag-unnecessary-face ((t :inherit unspecified)))
       ;; Flymake
       `(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-subtle-red))
       `(flymake-warning-bitmap '(exclamation-mark modus-themes-subtle-yellow))
       `(flymake-note-bitmap '(exclamation-mark modus-themes-subtle-cyan))
       ;; Evil
       `(evil-ex-substitute-matches ((t :background ,bg-changed :foreground ,fg-changed :underline t)))
       `(evil-ex-substitute-replacement ((t :background ,bg-added :foreground ,fg-added :underline t)))
       ;; Ansi
       `(ansi-color-faint ((t :foreground ,fg-dim :weight unspecified)))
       )))

  (add-hook 'modus-themes-after-load-theme-hook 'sm/customize-modus)

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
    (let ((appearance (sm/get-system-appearance)))
      (message "syncing theme: %s" appearance)
      (if (string= appearance "dark")
          (modus-themes-select 'modus-vivendi)
        (modus-themes-select 'modus-operandi))))

  (modus-themes-load-theme 'modus-operandi))

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

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
