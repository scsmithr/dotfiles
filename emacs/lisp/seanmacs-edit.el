;;; seanmacs-edit.el --- Edit -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing configuration.

;;; Code:

(setq inhibit-startup-message t
      use-dialog-box nil
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

;; Scrolling
(setq-default scroll-step 1
              scroll-margin 0
              scroll-conservatively 101
              hscroll-margin 0
              hscroll-step 1)

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(3)
      mouse-wheel-progressive-speed nil)

;; Configure file backups
(setq backup-directory-alist '((".*" . "~/.emacs.d/.backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/.backups" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; #Don't #create #lock #files
(setq create-lockfiles nil)

;; Auto insert closing parenthesis, braces, etc
(electric-pair-mode 1)

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

(setq-default require-final-newline t)

;; Always use "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Tab stuff
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Don't wrap lines
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(
                           face
                           space-mark
                           tab-mark lines-tail
                           trailing
                           tabs
                           spaces))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package ibuffer
  :init
  (core/leader
   "ob" 'ibuffer))

(use-package dtrt-indent
  :straight t
  :init
  (setq dtrt-indent-min-quality 65.0)
  (setq dtrt-indent-min-hard-tab-superiority 180.0)
  :config
  (add-to-list 'dtrt-indent-hook-mapping-list
               '(web-mode javascript web-mode-code-indent-offset))
  (dtrt-indent-global-mode 1))

(use-package projectile
  :straight t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode +1)
  (core/leader
   "p" 'projectile-command-map))

(use-package ripgrep
  :straight t
  :after projectile
  :config
  (shackle '(("^\\*ripgrep-search\\*"
              :action seanmacs/display-buffer-bottom
              :height 0.2))))

(use-package flycheck
  :straight t
  :config
  (core/leader
   "f" flycheck-command-map)
  (shackle '(("^\\*Flycheck errors"
              :action seanmacs/display-buffer-bottom
              :height 0.2)))
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-indication-mode 'right-fringe))

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

(use-package which-key
  :straight t
  :init
  (which-key-mode 1))

(use-package ido-vertical-mode
  :straight t
  :init
  (setq ido-enable-flex-matching t
        ido-vertical-define-keys 'C-n-and-C-p-only
        ido-vertical-indicator " >")
  :config
  (ido-mode 1)
  (ido-vertical-mode 1))

(use-package flx-ido
  :straight t
  :after ido-vertical-mode
  :config
  (setq ido-use-faces nil)
  :init
  (ido-everywhere 1)
  (flx-ido-mode 1))

(defun treemacs-set-modeline ()
  (setq mode-line-format " Seanmacs"))

(use-package treemacs
  :straight t
  :after doom-themes
  :init
  (setq treemacs-width 24)
  (setq treemacs-no-png-images t)
  ;; Allows ediff to take over the rest of the emacs frame. Uforturnately also
  ;; means I can't use evil-move-left to get to it anymore.
  (setq treemacs-is-never-other-window t)
  :config
  (core/leader
   "tt" 'treemacs-select-window
   "te" 'treemacs-edit-workspaces
   "tf" 'treemacs-finish-edit
   "tn" 'treemacs
   "ta" 'treemacs-add-and-display-current-project)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (add-hook 'treemacs-mode-hook #'treemacs-set-modeline))

(after! treemacs
        (treemacs-create-theme "minimal"
          :icon-directory (f-join treemacs-dir "icons/default")
          :config
          (progn
            (treemacs-create-icon :file "" :extensions (root) :fallback " ")
            (treemacs-create-icon :file "" :extensions (dir-closed) :fallback "")
            (treemacs-create-icon :file "" :extensions (dir-open) :fallback "")
            (treemacs-create-icon :file "" :extensions (tag-leaf) :fallback "")
            (treemacs-create-icon :file "" :extensions (tag-open) :fallback "")
            (treemacs-create-icon :file "" :extensions (tag-closed) :fallback "")
            (treemacs-create-icon :file "" :extensions (error) :fallback "")
            (treemacs-create-icon :file "" :extensions (warning) :fallback "")
            (treemacs-create-icon :file "" :extensions (info) :fallback "")
            (treemacs-create-icon :file "" :extensions (fallback) :fallback "")))
        (treemacs-load-theme "minimal"))

(use-package treemacs-evil
  :after treemacs evil
  :straight t)

(use-package treemacs-projectile
  :after treemacs projectile
  :straight t)

(use-package xref
  :config
  (shackle '(("^\\*xref"
              :action seanmacs/display-buffer-bottom
              :height 0.2))))

(provide 'seanmacs-edit)
;;; seanmacs-edit.el ends here
