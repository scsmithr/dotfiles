;;; ui.el --- UI

;; Default font
(set-frame-font (font-spec :family "Source Code Pro Medium"))
(face-attr 'default :height 110)

(setq use-dialog-box nil)
(setq inhibit-startup-message t
      inihibit-startup-echo-area-message t)

;; Highlight parenthesis
(show-paren-mode 1)

;; Resize fringe
(fringe-mode '(4 . 4))

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width-start t)

;; Highlight current line
(global-hl-line-mode +1)

;; Wrap column
(setq-default fill-column 80)

;; Whitespace stuff
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(
                         face
                         space-mark
                         tab-mark lines-tail
                         trailing
                         tabs
                         spaces))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Scrolling
(setq-default scroll-step 1)
(setq-default scroll-margin 4)
(setq-default scroll-conservatively 101)
(setq-default hscroll-margin 0)
(setq-default hscroll-step 1)

;; ediff settings
(winner-mode t)
(add-hook 'ediff-quit-hook #'winner-undo)
(add-hook 'ediff-prepare-buffer-hook #'show-all)
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-merge-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; Always use "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Tab stuff
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Don't wrap lines
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(setq-default require-final-newline t)

;; Configure file backups
(setq backup-directory-alist '((".*" . "~/.emacs.d/.backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/.backups" t)))
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; #Don't #create #lock #files
;; TODO: Doesn't work
(setq create-lockfiles nil)

;; Auto insert closing parenthesis, braces, etc
(electric-pair-mode 1)

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

(defun doom-transparentize (color alpha)
  "Transparentize a COLOR (a hexidecimal string) by a coefficient
ALPHA (a float between 0 and 1)."
  (doom-blend (doom-color color) (doom-color 'bg) (- 1 alpha)))

;; Doom themes
(use-package doom-themes
  :straight t
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-solarized-light t)
  (face-attr 'fringe :background (doom-color 'bg))
  (face-attr 'whitespace-tab :background "inherit")
  (face-attr 'font-lock-comment-face :foreground (doom-lighten 'fg 0.15))
  (face-attr 'font-lock-doc-face :foreground (doom-lighten 'fg 0.15))
  (face-attr 'whitespace-line
             :weight 'normal
             :foreground 'unspecified)
  (face-attr 'show-paren-match
             :weight 'bold
             :background (doom-transparentize 'cyan 0.5)
             :foreground (doom-color 'cyan))
  (face-attr 'line-number :foreground (doom-color 'fg-alt)))

(use-package treemacs
  :straight t
  :after doom-themes
  :init
  (setq treemacs-width 22)
  (setq treemacs-no-png-images t)
  ;; Allows ediff to take over the rest of the emacs frame. Uforturnately also
  ;; means I can't use evil-move-left to get to it anymore.
  (setq treemacs-is-never-other-window t)
  (setq treemacs-icon-tag-node-open-text (propertize "− " 'face 'font-lock-keyword-face)
        treemacs-icon-tag-node-closed-text (propertize "+ " 'face 'font-lock-keyword-face)
        treemacs-icon-tag-leaf-text (propertize "  " 'face 'font-lock-keyword-face))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (face-attr 'treemacs-fringe-indicator-face :background (doom-color 'blue))
  (face-attr 'treemacs-directory-face :foreground (doom-color 'blue))
  (face-attr 'treemacs-term-node-face :foreground (doom-color 'blue) :weight 'normal)
  (face-attr 'treemacs-git-modified-face :foreground (doom-color 'yellow))
  (face-attr 'treemacs-git-untracked-face :foreground (doom-color 'green))
  (face-attr 'treemacs-git-ignored-face :foreground (doom-color 'fg-alt))
  (face-attr 'treemacs-root-face
             :height 110
             :weight 'normal
             :foreground (doom-color 'green))
  (add-hook 'treemacs-mode-hook (lambda () (setq mode-line-format " Seanmacs")))
  (core/leader
   "tt" 'treemacs-select-window
   "te" 'treemacs-edit-workspaces
   "tf" 'treemacs-finish-edit
   "tn" 'treemacs
   "ta" 'treemacs-add-and-display-current-project))

(use-package treemacs-evil
  :after treemacs evil
  :straight t)

(use-package treemacs-projectile
  :after treemacs projectile
  :straight t)

(use-package which-key
  :straight t
  :init
  (which-key-mode 1))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (face-attr 'diff-hl-insert
             :foreground (doom-color 'green)
             :background (doom-color 'green))
  (face-attr 'diff-hl-delete
             :foreground (doom-color 'red)
             :background (doom-color 'red))
  (face-attr 'diff-hl-change
             :foreground (doom-color 'yellow)
             :background (doom-color 'yellow)))

(use-package ido-vertical-mode
  :straight t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-indicator " >")
  :config
  (ido-mode 1)
  (ido-vertical-mode 1))

(use-package flx-ido
  :straight t
  :after ido-vertical-mode
  :config
  (setq ido-use-faces nil)
  (set-face-attribute 'flx-highlight-face nil :foreground (doom-color 'magenta))
  :init
  (ido-everywhere 1)
  (flx-ido-mode 1))

;; Taken from https://wikemacs.org/wiki/Imenu
;; Allows for similar functionality to vs code's jump to symbol.
(defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Jump to symbol: " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

(core/leader "o" 'ido-goto-symbol)

(provide 'ui)
;;; ui.el ends here
