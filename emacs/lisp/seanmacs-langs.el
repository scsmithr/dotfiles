;;; seanmacs-langs.el --- Langs -*- lexical-binding: t; -*-

;;; Commentary:
;; Language configuration.

;;; Code:

;; General keymap rules:
;; All mode specific keymaps live under the local leader prefix.
;;
;; 'o' - Open repl
;;
;; 's' - Send to repl prefix
;;
;; 't' - Test prefix
;; 'tt' - Test current package/module/file
;; 'tp' - Test everything from project root
;; 'ti' - Interactively run test
;;
;; 'v' - Dependency vendoring
;;
;; 'r' - Rename prefix
;; 'rn' - Rename symbol
;; 'rf' - Rename file



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
            "tt" 'go/go-test-current-pkg
            "tp" 'go/go-test-root
            "ti" 'go/go-test-run
            "v" 'go/go-vendor)

(defvar go-test-buffer-name "*go test*"
  "Name of buffer for go test output.")

(defvar go-vendor-buffer-name "*go vendor*"
  "Name of buffer for go test output.")

(defun go/spawn (cmd buf-name)
  (compilation-start cmd nil (lambda (_n) buf-name)))

(defun go/go-test-current-pkg ()
  (interactive)
  (go/spawn "go test" go-test-buffer-name))

(defun go/go-test-root ()
  (interactive)
  (let ((cmd (format "cd %s; go test ./..." (projectile-project-root))))
    (go/spawn cmd go-test-buffer-name)))

(defun go/go-test-run (test-name)
  (interactive (list (read-string "Test name: ")))
  (let ((cmd
         (format "cd %s; go test -run %s ./..."
                 (projectile-project-root)
                 test-name)))
    (go/spawn cmd go-test-buffer-name)))

(defun go/go-vendor ()
  (interactive)
  (let ((cmd (format "cd %s; go mod vendor" (projectile-project-root))))
    (go/spawn cmd go-vendor-buffer-name)))

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
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package cargo
  :straight t
  :defer t)

(core/local 'rust-mode-map
            "rn" 'lsp-rename
            "tp" 'cargo-process-test
            "tt" 'cargo-process-current-file-tests)

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

(use-package ess
  :straight t
  :mode ("\\.jl\\'" . ess-julia-mode)
  :defer t
  :config
  ;; Force usage of colors, looks prettier and helps with spotting
  ;; errors/warnings with the @code_warntype macro.
  (setq inferior-julia-args "--color=yes"))

(core/local 'ess-julia-mode-map
            "o" 'julia)

;; Clojure

(use-package clojure-mode
  :straight t
  :defer t)

(use-package cider
  :straight t
  :defer t
  :config
  (evil-add-command-properties #'cider-find-var :jump t))

;; Emacs lisp

(use-package elisp-mode
  ;; built-in
  :defer t
  :config
  (shackle '((inferior-emacs-lisp-mode :height 0.5))))

(core/local 'emacs-lisp-mode-map
            "o" 'ielm)

;; C/C++

(use-package ccls
  :straight t
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda ()
           (require 'ccls)
           (lsp))))

;; Yaml

(use-package yaml-mode
  :straight t)

;; Markdown

(use-package markdown-mode
  :straight t
  :defer t
  :config
  (evil-collection-define-key 'normal 'markdown-mode-map
    (kbd "TAB") 'markdown-cycle))

;; Dockerfile

(use-package dockerfile-mode
  :straight t)

(provide 'seanmacs-langs)
;;; seanmacs-langs.el ends here
