;; Hide some things
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
      inihibit-startup-echo-area-message t)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; Fringe widths, git/flycheck
(fringe-mode '(4 . 4))

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width-start t)

;; Highlight current line
(global-hl-line-mode +1)

;; Tab stuff
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

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

;; Don't wrap lines
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(setq scroll-step 1)
(setq scroll-margin 4)

(setq require-final-newline t)

;; Configure file backups
(setq backup-directory-alist '(("." . "~/.emacs.d/.backups")))
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; #Don't #create #lock #files
(setq create-lockfiles nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit git-gutter-fring doom-modeline rust-mode haskell-mode git-gutter-fringe which-key flx-ido web-mode tide flycheck lsp-mode go-mode treemacs-projectile treemacs-evil treemacs projectile ido-vertical-mode evil use-package))))

(set-face-attribute 'default nil :font "Source Code Pro" :height 110)

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Get use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; evil
(use-package evil
  :ensure t
  :init
  (setq evil-search-module'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  ;; (bind-key* "C p" 'projectile-find-file)
  ;; Set up leader key (defvar leader-map
  (defvar leader-map (make-sparse-keymap) "Keymap for leader key")
  (define-key evil-normal-state-map "," leader-map)
  (define-key leader-map "w" 'evil-window-vsplit)
  (define-key leader-map "h" 'evil-window-split)
  ;; Rebind ctrl-p
  (define-key evil-normal-state-map (kbd "C-p") #'projectile-find-file))

(use-package evil-commentary
  :ensure t
  :init
  (evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Doom themes
(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-one t)
  (load-user-file "modeline.el")
  (set-face-attribute 'whitespace-tab nil :background "inherit")
  (set-face-attribute 'line-number nil :foreground (doom-color 'fg-alt)))

;; Vertical ido
(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-mode 1)
  (ido-vertical-mode 1))

(use-package flx-ido
  :ensure t
  :after ido-vertical-mode
  :config
  (setq ido-use-faces nil)
  :init
  (ido-everywhere 1)
  (flx-ido-mode 1))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode +1)
  (define-key leader-map "p" 'projectile-command-map))

(use-package treemacs
  :after doom-themes
  :ensure t
  :init
  (setq treemacs-width 25)
  (setq treemacs-no-png-images t)
  :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
  (set-face-attribute 'treemacs-directory-face nil :foreground (doom-color 'fg))
  (set-face-attribute 'treemacs-term-node-face nil :foreground (doom-color 'fg-alt))
  (set-face-attribute 'treemacs-git-modified-face nil :foreground (doom-color 'yellow))
  (set-face-attribute 'treemacs-git-untracked-face nil :foreground (doom-color 'magenta))
  (set-face-attribute 'treemacs-root-face nil :foreground (doom-color 'blue))
  (define-key leader-map "n" 'treemacs)
  (define-key leader-map "a" 'treemacs-add-and-display-current-project))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  )

(use-package lsp-ui :commands lsp-ui-mode)

;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
  )

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-indication-mode 'right-fringe)
  (define-key leader-map "l" 'flycheck-list-errors)
  (set-face-attribute 'flycheck-fringe-info nil :foreground (doom-color 'bg) :background (doom-color 'bg))
  (set-face-attribute 'flycheck-fringe-warning nil :foreground (doom-color 'orange) :background (doom-color 'orange))
  (set-face-attribute 'flycheck-fringe-error nil :foreground (doom-color 'red) :background (doom-color 'red))
  :init (global-flycheck-mode))

(use-package company
  :ensure t
  :config
  (setq company-frontends '(company-preview-frontend company-echo-frontend))
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 1)
  (define-key company-active-map (kbd "<return>") #'company-complete-selection)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package haskell-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :init)

(use-package typescript-mode
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (add-hook 'typescript-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'typescript-mode))


(use-package which-key
  :ensure t
  :init
  (which-key-mode 1))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t))

(use-package git-gutter-fringe
  :ensure t
  :init
  :config
  (global-git-gutter-mode)
  (set-face-attribute 'git-gutter-fr:modified nil :foreground (doom-color 'blue) :background (doom-color 'blue))
  (set-face-attribute 'git-gutter-fr:added nil :foreground (doom-color 'green) :background (doom-color 'green))
  (set-face-attribute 'git-gutter-fr:deleted nil :foreground (doom-color 'magenta) :background (doom-color 'magenta)))

(use-package magit
  :ensure t)

