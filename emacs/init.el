;; Hide some things.
;; Do this first so that I never see the menu bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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
    (company-box alchemist elixir-mode prettier-js ob-http ob-restclient restclient company-posframe esup go-rename docker-tramp forge protobuf-mode evil-magit diff-hl dtrt-indent yasnippet ripgrep idomenu swoop lsp-ui company company-lsp magit git-gutter-fring doom-modeline rust-mode haskell-mode git-gutter-fringe which-key flx-ido web-mode tide flycheck lsp-mode go-mode treemacs-projectile treemacs-evil treemacs projectile ido-vertical-mode evil use-package))))

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Get use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(defmacro after! (target &rest body)
  "Load BODY after TARGET."
  `(with-eval-after-load ',target ,@body))

(defun face-attr (face &rest args)
  (apply #'set-face-attribute face nil args))

;; Core configuration

(use-package keybinds
  :load-path "lisp"
  :config
  (core/init-leader))

(use-package ui
  :load-path "lisp")

(use-package completions
  :load-path "lisp")

(use-package utils
  :load-path "lisp")

(use-package orgmode
  :load-path "lisp")

(use-package modeline
  :load-path "lisp"
  :config
  (modeline-mode))

(use-package langs
  :load-path "lisp")

;; Useful functions

