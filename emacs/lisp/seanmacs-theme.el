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
    (set-face-attribute 'fixed-pitch nil :family "PragmataPro Mono")
    (set-face-attribute 'variable-pitch nil :family "Source Sans 3" :height 160)
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
  :straight (:host sourcehut :repo "protesilaos/modus-themes" :branch "version-4")
  :config

  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only)
        modus-themes-org-blocks 'gray-background
        modus-themes-prompts '(bold))

  (setq modus-themes-common-palette-overrides
        '(
          (fringe bg-inactive)
          (cursor fg-main)
          (builtin fg-main)
          (comment yellow-cooler)
          (constant fg-main)
          (docstring green-warmer)
          (docmarkup green-cooler)
          (fnname fg-main)
          (keyword fg-dim)
          (preprocessor magenta-faint)
          (string blue-faint)
          (type fg-main)
          (variable fg-main)
          (rx-construct fg-main)
          (rx-backslash fg-main)

          (keybind blue-cooler)
          (fg-link blue-faint)
          (fg-link-symbolic cyan-faint)
          (fg-link-visited magenta-faint)
          (name magenta)
          (identifier yellow-cooler)
          (prompt blue-faint)

          (err red)
          (warning yellow-warmer)
          (info green)

          (underline-err red-intense)
          (underline-warning yellow-intense)
          (underline-note cyan-intense)

          (fg-accent-0 blue-faint)
          (fg-accent-1 magenta-faint)
          (fg-accent-2 cyan-faint)
          (fg-accent-3 red-faint)
          (bg-accent-0 bg-blue-subtle)
          (bg-accent-1 bg-magenta-subtle)
          (bg-accent-2 bg-cyan-subtle)
          (bg-accent-3 bg-red-subtle)

          (date-common cyan-faint)
          (date-deadline red)
          (date-event fg-alt)
          (date-holiday magenta)
          (date-scheduled yellow-warmer)
          (date-weekend red-faint)

          (mail-cite-0 blue-faint)
          (mail-cite-1 yellow-warmer)
          (mail-cite-2 cyan-cooler)
          (mail-cite-3 red-cooler)
          (mail-part cyan)
          (mail-recipient magenta-cooler)
          (mail-subject magenta-warmer)
          (mail-other magenta-faint)

          (prose-block fg-dim)
          (prose-code green-warmer)
          (prose-done green-warmer)
          (prose-macro magenta-warmer)
          (prose-metadata fg-dim)
          (prose-metadata-value fg-alt)
          (prose-table fg-alt)
          (prose-tag magenta-faint)
          (prose-todo red-warmer)
          (prose-verbatim magenta-warmer)

          (fg-heading-0 fg-main)
          (fg-heading-1 fg-main)
          (fg-heading-2 fg-main)
          (fg-heading-3 fg-main)
          (fg-heading-4 fg-main)
          (fg-heading-5 fg-main)
          (fg-heading-6 fg-main)
          (fg-heading-7 fg-main)
          (fg-heading-8 fg-main)

          ;; Mode line
          (bg-mode-line-active        bg-active)
          (fg-mode-line-active        fg-main)
          (border-mode-line-active    border)
          (bg-mode-line-inactive      bg-inactive)
          (fg-mode-line-inactive      fg-dim)
          (border-mode-line-inactive  border)
          ))

  (setq modus-vivendi-palette-overrides
        '(
          (bg-main          "#292626")
          (bg-dim           "#353333")
          (fg-dim           "#b8b8b8")
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
       `(region ((,c :distant-foreground unspecified
                     ,@(modus-themes--region bg-region fg-main bg-region-subtle))))
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
