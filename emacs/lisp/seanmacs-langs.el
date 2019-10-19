;;; seanmacs-langs.el --- Langs -*- lexical-binding: t; -*-

;;; Commentary:
;; Language configuration.

;;; Code:

(use-package go-mode
  :straight t
  :defer t
  :config
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (evil-add-command-properties #'godef-jump :jump t)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq company-backends (delete 'company-capf company-backends))))
  (setq gofmt-command "goimports"))

(core/local 'go-mode-map
            "rn" 'lsp-rename
            "ta" 'go/go-tests-all
            "tv" 'go/go-tests-all-verbose
            "v" 'go/go-vendor
            "fr" 'lsp-find-references)

(defvar go-test-buffer-name "*go test*"
  "Name of buffer for go test output.")

(defvar go-vendor-buffer-name "*go vendor*"
  "Name of buffer for go test output.")

(defun go/go-tests (args)
  (interactive)
  (compilation-start (concat "cd " (projectile-project-root)
                             " && go test " args)
                     nil (lambda (n) go-test-buffer-name) nil))

(defun go/go-tests-all ()
  (interactive)
  (go/go-tests "./..."))

(defun go/go-tests-all-verbose ()
  (interactive)
  (go/go-tests "./... -v"))

(defun go/go-vendor ()
  (interactive)
  (compilation-start (concat "cd " (projectile-project-root)
                             " && go mod vendor")
                     nil (lambda (n) go-vendor-buffer-name) nil))

;; Haskell

(use-package haskell-mode
  :straight t
  :defer t
  :config
  (setq haskell-stylish-on-save t)
  (setq haskell-mode-stylish-haskell-path "brittany")
  (shackle '(("^\\*haskell\\*" :height 0.5))))

(core/local 'haskell-mode-map
            "o" 'run-haskell)

;; Octave

(use-package octave
  :defer t
  :mode ("\\.m\\'" . octave-mode))

(core/local 'octave-mode-map
            "o" 'run-octave
            "sr" 'octave-send-region
            "sb" 'octave-send-buffer
            "sl" 'octave-send-line)

;; Rust

(use-package rust-mode
  :straight t
  :defer t
  :config
  (setq rust-format-on-save t)
  (setq lsp-rust-clippy-preference "on")
  :hook (rust-mode . lsp))

;; Typescript

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package tide
  :straight t
  :after (web-mode company flycheck))

(use-package prettier-js
  :straight t
  :after (web-mode))

(use-package web-mode
  :straight t
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-block-padding 4
        web-mode-comment-style 1

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-indentation nil)
  (add-hook 'web-mode-hook #'use-eslint-from-node-modules)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-match-p "tsx?" (file-name-extension buffer-file-name))
                (setup-tide-mode)
                (evil-add-command-properties #'tide-jump-to-definition :jump t)
                (prettier-js-mode)
                (flycheck-add-mode 'javascript-eslint 'web-mode)
                (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)))))

(core/local 'web-mode-map
            "rn" 'tide-rename-symbol
            "rf" 'tide-rename-file)

;; Elixir

(use-package elixir-mode
  :straight t
  :defer t
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (evil-add-command-properties #'alchemist-goto-defintion-at-point :jump t))

(use-package alchemist
  :straight t
  :defer t)

(core/local 'elixir-mode-map
            "mc" 'alchemist-mix-compile
            "mr" 'alchemist-mix-run)

;; Protobuf

(use-package protobuf-mode
  :straight t
  :defer t)

;; Common lisp

(use-package slime
  :straight t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(core/local 'lisp-mode-map
            "o" 'slime)

;; Julia

(use-package julia-mode
  :straight t
  :interpreter "julia"
  :defer t
  :config
  (shackle '(("^\\*Julia\\*" :height 0.5))))

(core/local 'julia-mode-map
            "o" 'run-julia)

;; C/C++

(use-package ccls
  :straight t
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda ()
           (require 'ccls)
           (lsp))))

(provide 'seanmacs-langs)
;;; seanmacs-langs.el ends here
