;; Language related modes

(require 'use-package)

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'lsp))

;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'lsp))

;; Haskell
(use-package haskell-mode
  :ensure t)

;; General html, javascript, etc.
(use-package web-mode
  :ensure t)

;; Typescript
(use-package typescript-mode
  :ensure t
  :init
  (add-hook 'typescript-mode-hook #'lsp))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
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

