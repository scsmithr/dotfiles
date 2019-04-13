;; Hide some things
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
      inihibit-startup-echo-area-message t)

;; Line numbers
(add-hook 'prog-mode-hook 'linum-mode)

;; Highlight current line
(global-hl-line-mode +1)

;; Tab stuff
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

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
    (web-mode tide flycheck lsp-mode go-mode treemacs-projectile treemacs-evil treemacs projectile ido-vertical-mode evil use-package))))

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
  ;; Set up leader key (defvar leader-map
  (defvar leader-map (make-sparse-keymap) "Keymap for leader key")
  (define-key evil-normal-state-map "," leader-map)
  (define-key leader-map "w" 'evil-window-vsplit))

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
  (load-theme 'doom-one t))

;; Vertical ido
(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-mode 1)
  (ido-vertical-mode 1))

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
  (setq treemacs-width 20)
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
  (set-face-attribute 'treemacs-term-node-face nil :foreground (doom-color 'blue))
  (set-face-attribute 'treemacs-git-modified-face nil :foreground (doom-color 'yellow))
  (set-face-attribute 'treemacs-git-untracked-face nil :foreground (doom-color 'green))
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
  (setq flycheck-indication-mode nil)
  :init (global-flycheck-mode))

(use-package company
  :ensure t
  :config
  (setq company-frontends '(company-preview-frontend company-echo-frontend))
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 1)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package web-mode
  :ensure t
  :init)

(use-package tide
  :ensure t
  :after web-mode
  :init
  (defun setup-tide-mode ()
      (interactive)
       (tide-setup)
       (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
       (eldoc-mode +1)
       (company-mode +1)
       (tide-hl-identifier-mode +1))

    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)

    ;; formats the buffer before saving
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)

    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-hook 'typescript-mode-hook
	      (lambda ()
		(when (string-equal "tsx" (file-name-extension buffer-file-name))
		    (setup-tide-mode))))
    ;; funky typescript linting in web-mode
    (flycheck-add-mode 'typescript-tslint 'web-mode))


