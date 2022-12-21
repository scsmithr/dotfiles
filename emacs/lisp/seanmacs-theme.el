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
        modus-themes-org-blocks 'gray-background)

  ;; Just overriding a single theme.
  (setq modus-vivendi-palette-overrides
        '(
          (bg-main          "#292626")
          (bg-dim           "#353333")
          (fg-main          "#f0dbdb")
          (fg-dim           "#9b9898")
          (fg-alt           "#c7daff")
          (bg-active        "#535353")
          (bg-inactive      "#303030")
          (border           "#646464")

          (fringe bg-inactive)            ; set to bg-main to hide it
          (cursor fg-main)
          (builtin maroon)
          (comment fg-dim)                ; default
          (constant blue-faint)
          (docstring yellow-faint)
          (docmarkup magenta-faint)       ; default
          (fnname pink)
          (keyword magenta-faint)
          (preprocessor rust)
          (string slate)
          (type cyan-faint)
          (variable cyan-faint)
          (rx-construct gold)
          (rx-backslash olive)

          (underline-err red-faint)
          (underline-warning yellow-faint)
          (underline-note cyan-faint)

          (date-common slate)
          (date-deadline rust)
          (date-event fg-alt)             ; default
          (date-holiday magenta)          ; default
          (date-scheduled yellow-faint)
          (date-weekend pink)

          (link blue-faint)
          (link-symbolic cyan-faint)
          (link-visited magenta-faint)
          (name maroon)
          (identifier yellow-faint)       ; default
          (prompt cyan-faint)

          (mail-cite-0 cyan-faint)
          (mail-cite-1 yellow-faint)
          (mail-cite-2 green-faint)
          (mail-cite-3 red-faint)
          (mail-part olive)
          (mail-recipient indigo)
          (mail-subject maroon)
          (mail-other slate)

          (prose-block fg-dim)            ; default
          (prose-code olive)
          (prose-done green-faint)
          (prose-macro indigo)
          (prose-metadata fg-dim)         ; default
          (prose-metadata-value fg-alt)   ; default
          (prose-table fg-alt)            ; default
          (prose-tag rust)
          (prose-todo red-faint)
          (prose-verbatim maroon)

          (accent-0 blue-faint)
          (accent-1 magenta-faint)
          (accent-2 cyan-faint)
          (accent-3 yellow)
          (bg-accent-0 bg-blue-subtle)
          (bg-accent-1 bg-magenta-subtle)
          (bg-accent-2 bg-cyan-subtle)
          (bg-accent-3 bg-yellow-subtle)

          ;; All headings are left at their defaults values.

          (heading-0 cyan-cooler)
          (heading-1 fg-main)
          (heading-2 yellow-faint)
          (heading-3 blue-faint)
          (heading-4 magenta)
          (heading-5 green-faint)
          (heading-6 red-faint)
          (heading-7 cyan-faint)
          (heading-8 fg-dim)

          ;; Special purpose

          (bg-completion       "#5f446f")
          (bg-hover            "#004f70")
          (bg-hover-secondary  "#654a39")
          (bg-hl-line          "#3f3849")
          (bg-paren-match      "#2f7f9f")
          (bg-paren-expression "#453040")
          (bg-region           "#5c5c5c")
          (bg-region-subtle    "#4f1c2f")
          (bg-prompt           "#5f3a60")

          (bg-mode-line-active        "#555252")
          (fg-mode-line-active        fg-main)
          (border-mode-line-active    "#a09797")
          (bg-mode-line-inactive      "#332f2f")
          (fg-mode-line-inactive      fg-dim)
          (border-mode-line-inactive  "#666363")
          ))

  (setq modus-operandi-tinted-palette-overrides
        '(
          (comment             fg-dim)
          (bg-hl-line          "#f0e5e0")
          (bg-completion       "#ecd5d0")
          ))

  (defun sm/customize-modus ()
    (modus-themes-with-colors
      (custom-set-faces
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
       ;; Evil
       `(evil-ex-substitute-matches ((t :background ,bg-changed :underline nil)))
       `(evil-ex-substitute-replacement ((t :background ,bg-added)))
       ;; Diff hl
       `(diff-hl-change ((t :background unspecified :foreground ,fg-changed)))
       `(diff-hl-delete ((t :background unspecified :foreground ,fg-removed)))
       `(diff-hl-insert ((t :background unspecified :foreground ,fg-added)))
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
        (modus-themes-select 'modus-operandi-tinted))))

  (modus-themes-load-theme 'modus-operandi-tinted))

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
