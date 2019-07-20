;; Hide some things
(menu-bar-mode -1)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
(setq inhibit-startup-message t
      inihibit-startup-echo-area-message t)

;; Highlight parenthesis
(show-paren-mode 1)

(fringe-mode '(4 . 4))
;; Auto insert closing parenthesis, braces, etc
(electric-pair-mode 1)

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
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

(setq-default scroll-step 1)
(setq-default scroll-margin 4)
(setq-default scroll-conservatively 101)
(setq-default hscroll-margin 0)
(setq-default hscroll-step 1)

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

;; Where org-captures go.
(setq org-default-notes-file "~/notes/refile.org")
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-startup-folded nil)

(add-hook 'org-mode-hook (lambda ()
                           (org-babel-do-load-languages 'org-babel-load-languages
                                                        (append org-babel-load-languages
                                                                '((shell . t)
                                                                  (restclient . t)
                                                                  (http . t))))))

;; ediff settings
(winner-mode t)
(add-hook 'ediff-quit-hook #'winner-undo)
(add-hook 'ediff-prepare-buffer-hook #'show-all)
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-merge-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; Always use "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

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
    (alchemist elixir-mode prettier-js ob-http ob-restclient restclient company-posframe esup go-rename docker-tramp forge protobuf-mode evil-magit diff-hl dtrt-indent yasnippet ripgrep idomenu swoop lsp-ui company company-lsp magit git-gutter-fring doom-modeline rust-mode haskell-mode git-gutter-fringe which-key flx-ido web-mode tide flycheck lsp-mode go-mode treemacs-projectile treemacs-evil treemacs projectile ido-vertical-mode evil use-package))))

(set-frame-font (font-spec :family "Source Code Pro Medium"))
(set-face-attribute 'default nil :height 110)

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Get use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Core configuration

(use-package keybinds
  :load-path "core"
  :after evil
  :config
  (core/init-leader))

;; External packages

(defun evil-window-next-skip-treemacs ()
  "Move the cursor to next window in cyclic order, skipping
Treemacs buffers."
  (interactive)
  (select-window (next-window))
  (when (string-match-p (regexp-quote "Treemacs") (buffer-name))
    (select-window (next-window))))

;; evil
(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  (core/leader
   "ww" 'evil-window-vsplit
   "wh" 'evil-window-split)
  ;; Overwrite the default next window commands with one that skips treemacs.
  (define-key evil-window-map "C-w" #'evil-window-next-skip-treemacs)
  (define-key evil-window-map "w" #'evil-window-next-skip-treemacs)
  (define-key evil-motion-state-map (kbd "C-w w") #'evil-window-next-skip-treemacs)
  (define-key evil-motion-state-map (kbd "C-w C-w") #'evil-window-next-skip-treemacs)
  ;; rebind ctrl-p
  (define-key evil-normal-state-map (kbd "C-p") #'projectile-find-file))

(use-package evil-commentary
  :ensure t
  :after evil
  :init
  (evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(defun doom-transparentize (color alpha)
  "Transparentize a COLOR (a hexidecimal string) by a coefficient
ALPHA (a float between 0 and 1)."
  (doom-blend (doom-color color) (doom-color 'bg) (- 1 alpha)))

;; Doom themes
(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-solarized-light t)
  (set-face-attribute 'fringe nil :background (doom-color 'bg))
  (set-face-attribute 'whitespace-tab nil :background "inherit")
  (set-face-attribute 'font-lock-comment-face nil :foreground (doom-lighten 'fg 0.15))
  (set-face-attribute 'font-lock-doc-face nil :foreground (doom-lighten 'fg 0.15))
  (set-face-attribute 'whitespace-line nil
                      :weight 'normal
                      :foreground 'unspecified)
  (set-face-attribute 'show-paren-match nil
                      :weight 'bold
                      :background (doom-transparentize 'cyan 0.5)
                      :foreground (doom-color 'cyan))
  (set-face-attribute 'line-number nil :foreground (doom-color 'fg-alt)))

;; Auto detect indentation type/level
(use-package dtrt-indent
  :ensure t
  :init
  (setq dtrt-indent-min-quality 65.0)
  (setq dtrt-indent-min-hard-tab-superiority 180.0)
  :config
  (add-to-list 'dtrt-indent-hook-mapping-list
               '(web-mode javascript web-mode-code-indent-offset))
  (dtrt-indent-global-mode 1))

;; Vertical ido
(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-indicator " >")
  :config
  (ido-mode 1)
  (ido-vertical-mode 1))

(use-package flx-ido
  :ensure t
  :after ido-vertical-mode
  :config
  (setq ido-use-faces nil)
  (set-face-attribute 'flx-highlight-face nil :foreground (doom-color 'magenta))
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
  (core/leader
   "p" 'projectile-command-map))

(use-package ripgrep
  :ensure t
  :config
  (set-face-attribute 'ripgrep-match-face nil :foreground (doom-color 'yellow)))

(use-package treemacs
  :ensure t
  :after doom-themes
  :init
  (setq treemacs-width 22)
  (setq treemacs-no-png-images t)
  (setq treemacs-indentation 1)
  (setq treemacs-icon-tag-node-open-text (propertize "− " 'face 'font-lock-keyword-face)
        treemacs-icon-tag-node-closed-text (propertize "+ " 'face 'font-lock-keyword-face)
        treemacs-icon-tag-leaf-text (propertize "  " 'face 'font-lock-keyword-face))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (set-face-attribute 'treemacs-fringe-indicator-face nil :background (doom-color 'blue))
  (set-face-attribute 'treemacs-directory-face nil :foreground (doom-color 'blue))
  (set-face-attribute 'treemacs-term-node-face nil :foreground (doom-color 'blue) :weight 'normal)
  (set-face-attribute 'treemacs-git-modified-face nil :foreground (doom-color 'yellow))
  (set-face-attribute 'treemacs-git-untracked-face nil :foreground (doom-color 'green))
  (set-face-attribute 'treemacs-git-ignored-face nil :foreground (doom-color 'fg-alt))
  (set-face-attribute 'treemacs-root-face nil
                      :height 110
                      :weight 'normal
                      :foreground (doom-color 'green))
  (core/leader
   "t" treemacs-mode-map
   "n" 'treemacs
   "a" 'treemacs-add-and-display-current-project))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package flycheck
  :ensure t
  :after doom-themes
  :config
  (add-hook 'typescript-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'go-mode 'flycheck-mode)
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-indication-mode 'right-fringe)
  (core/leader "f" flycheck-command-map)
  (set-face-attribute 'flycheck-fringe-info nil
                      :foreground (doom-transparentize 'green 0.5)
                      :background (doom-transparentize 'green 0.5))
  (set-face-attribute 'flycheck-fringe-warning nil
                      :foreground (doom-transparentize 'orange 0.5)
                      :background (doom-transparentize 'orange 0.5))
  (set-face-attribute 'flycheck-fringe-error nil
                      :foreground (doom-transparentize 'red 0.5)
                      :background (doom-transparentize 'red 0.5)))

(use-package company
  :ensure t
  :config
  (setq company-frontends '(company-posframe-frontend))
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (set-face-attribute 'company-echo nil
                      :background "inherit"
                      :foreground (doom-color 'blue))
  (set-face-attribute 'company-echo-common nil
                      :background "inherit"
                      :weight 'bold
                      :foreground (doom-color 'orange))
  (define-key company-active-map (kbd "<return>") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-posframe
  :ensure t)

(use-package company-lsp
  :ensure t
  :after (company lsp)
  :config
  (add-to-list 'company-lsp-filter-candidates '(gopls . nil))
  (push 'company-lsp company-backends))

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (set-face-attribute 'diff-hl-insert nil
                      :foreground (doom-color 'green)
                      :background (doom-color 'green))
  (set-face-attribute 'diff-hl-delete nil
                      :foreground (doom-color 'red)
                      :background (doom-color 'red))
  (set-face-attribute 'diff-hl-change nil
                      :foreground (doom-color 'yellow)
                      :background (doom-color 'yellow)))

(use-package magit
  :ensure t
  :defer t
  :config
  (define-key magit-file-mode-map (kbd "C-c g") 'magit-file-dispatch)
  (core/leader
   "g" 'magit-status))

(use-package forge
  :ensure t
  :after magit)

(use-package evil-magit
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;; lsp
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-response-timeout 2)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-flycheck-enable t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package docker-tramp
  :ensure t
  :defer t)

(use-package restclient
  :ensure t)

(use-package ob-restclient
  :ensure t)

(use-package ob-http
  :ensure t)

;; Local configuration

(use-package modeline
  :load-path "lisp"
  :config
  (modeline-mode))

(use-package ido-symbol
  :load-path "lisp"
  :config
  (core/leader "o" 'ido-goto-symbol))

;; Language stuff

(use-package langs
  :load-path "lisp"
  :config
  (go/init)
  (rust/init)
  (octave/init)
  (haskell/init)
  (typescript/init-prettier)
  (typescript/init-web)
  (typescript/init-tide)
  (elixir/init))

;; Useful functions

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(core/leader "ci" 'indent-buffer)
