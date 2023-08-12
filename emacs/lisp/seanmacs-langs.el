;;; seanmacs-langs.el --- Langs -*- lexical-binding: t; -*-

;;; Commentary:
;; Language configuration.

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'treesit)


;; Go

(require 'go-ts-mode)

(use-package go-ts-mode
  :init
  (setq go-ts-mode-indent-offset 4)

  (defun sm/gopls-ensure ()
    (setq eglot-workspace-configuration
          `((gopls . (directoryFilters ,(vector "-node_modules"
                                                "-vendor")))))
    (eglot-ensure))
  :config
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)

  (defun sm/go-test-package ()
    "Run tests in the current package."
    (interactive)
    (let ((cmd (format "go test %s"
                       (file-name-directory buffer-file-name)))
          (buf-name "*Go Test*"))
      (sm/compile cmd buf-name)))

  :hook ((go-ts-mode . apheleia-mode)
         (go-ts-mode . sm/gopls-ensure))
  :bind(:map go-ts-mode-map
             ("C-c C-c C-t" . sm/go-test-package)))


;; Rust

(require 'rust-ts-mode)

(use-package rust-ts-mode
  :config
  (sm/add-server-program 'rust-ts-mode "rust-analyzer")

  (setf (alist-get 'rustfmt  apheleia-formatters)
        '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2021"))

  :hook ((rust-ts-mode . cargo-minor-mode)
         (rust-ts-mode . eglot-ensure)
         (rust-ts-mode . apheleia-mode)))

(use-package cargo
  :straight t
  :config
  (setq cargo-process--command-doc "doc --document-private-items"
        cargo-process--command-doc-open "doc --document-private-items --open")

  (setq cargo-process--command-clippy "clippy")

  (defun sm/set-cargo-process-scroll-bottom ()
    (setq-local compilation-scroll-output t))

  :hook ((cargo-process-mode . sm/set-cargo-process-scroll-bottom)
         (cargo-process-mode . visual-line-mode)))


;; Typescript/ web stuff

(require 'typescript-ts-mode)

;; `tsx-ts-mode' and `typescript-ts-mode' both derive from this.
(use-package typescript-ts-base-mode
  :init
  (setq typescript-ts-mode-indent-offset 4)
  :hook ((typescript-ts-base-mode . eglot-ensure)
         (typescript-ts-base-mode . apheleia-mode))
  :bind (:map typescript-ts-base-mode-map
              ("C-c C-d" . sm/eglot-lookup-doc)))

(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-comment-style 1
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-indentation nil))

(use-package css-mode
  ;; builtin
  :config
  (setq css-fontify-colors nil))


;; Protobuf

(use-package protobuf-mode
  :straight t
  :config
  (setf (alist-get 'protobuf-mode apheleia-mode-alist) 'clang-format)
  :hook ((protobuf-mode . apheleia-mode)))


;; Emacs lisp

(use-package elisp-mode
  ;; built-in
  :init
  (defun sm/elisp-describe-symbol-at-point ()
    (interactive)
    (let ((sym (symbol-at-point)))
      (describe-symbol sym)))

  (setq elisp-flymake-byte-compile-load-path nil)

  (defun sm/set-elisp-flymake-load-path ()
    (interactive)
    (setq-local elisp-flymake-byte-compile-load-path load-path))

  (defun sm/set-elisp-flymake-load-path-when-dots ()
    (when (string-prefix-p sm/dotfiles-dir buffer-file-name)
      (sm/set-elisp-flymake-load-path)))

  :hook ((emacs-lisp-mode . sm/set-elisp-flymake-load-path-when-dots))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-d" . sm/elisp-describe-symbol-at-point)
              :map lisp-interaction-mode-map
              ("C-c C-d" . sm/elisp-describe-symbol-at-point)))


;; Yaml

(require 'yaml-ts-mode)


;; Toml

(require 'toml-ts-mode)


;; Json

(require 'json-ts-mode)


;; Markdown

(use-package markdown-mode
  :straight t
  :mode ("\\.mdx?\\'" . gfm-mode)
  :config
  (setq-default markdown-hide-urls nil)
  (setq markdown-url-compose-char ?#
        markdown-fontify-code-blocks-natively t
        markdown-italic-underscore t
        markdown-spaces-after-code-fence 0
        markdown-gfm-use-electric-backquote nil)

  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . turn-on-auto-fill)))


;; Dockerfile

(require 'dockerfile-ts-mode)


;; TLA+

(define-derived-mode tlaplus-mode prog-mode "TLA+"
  "Mode for working with TLA+ files."
  (setq-local comment-start "(*"
              comment-end "*)"))

(add-to-list 'auto-mode-alist '("\\.tla\\'" . tlaplus-mode))

;; Nice to have things updated immediately when running 'pcal'.
(add-hook 'tlaplus-mode-hook #'auto-revert-mode)


;; C/C++

(require 'c-ts-mode)

(use-package c-ts-mode
  :config
  (sm/add-server-program 'c++-ts-mode "clangd")
  (sm/add-server-program 'c-ts-mode "clangd")

  :hook ((c++-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . apheleia-mode)
         (c-ts-mode . apheleia-mode)))


;; SQL

(use-package sql-indent
  :straight t)

(use-package sql
  ;; built-in
  :config

  (defun sm/sql-glaredb-local (&optional buf-name)
    "Connect to locally running glaredb instance."
    (interactive "P")
    (let ((sql-connection-alist '((glaredb-psql (sql-product 'postgres)
                                                (sql-database "glaredb")
                                                (sql-user "glaredb")
                                                (sql-password "")
                                                (sql-server "localhost")
                                                (sql-port 6543)))))
      (sql-connect 'glaredb-psql buf-name)))

  (defun sm/sql-postgres-generic (&optional buf-name)
    "Connect to a database via a postgres connection string."
    (interactive "P")
    (let ((sql-connection-alist '((generic-psql (sql-product 'postgres)
                                                (sql-database (read-string "Connection: "))
                                                (sql-user "")
                                                (sql-server "")
                                                (sql-port 0)))))
      (sql-connect 'generic-psql buf-name)))

  (advice-add 'sql-highlight-product :after #'sm/reinitialize-whitespace-mode)
  :hook ((sql-mode . sqlind-minor-mode)))

;; sqllogictest

(defvar sqllogictest-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Add '#' as a comment.
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\f ">" table)
    table)
  "Syntax table for `sqlogictest-mode'.")

(defvar sqllogictest-mode-font-lock-keywords
  '(("^\\(statement\\|query\\)" . font-lock-keyword-face)
    ("^----" . font-lock-doc-markup-face)
    ("^halt" . font-lock-warning-face)
    ("^hash-threshold" . font-lock-function-name-face)
    ("^\\(skipif\\|onlyif\\)" . font-lock-function-name-face))
  "Keywords specification for `sqllogictest-mode'.")

(defvar sqllogictest-mode-imenu-generic-expression
  '(("Statements.error" "^statement error\\(?:[:space:]\\)*[\r\n]\\(.+\\)" 1)
    ("Statements.ok" "^statement ok\\(?:[:space:]\\)*[\r\n]\\(.+\\)" 1)
    ("Queries" "^query .+[\r\n]\\(.+\\)" 1))
  "Imenu expression for `sqllogictest-mode'.")

(define-derived-mode sqllogictest-mode prog-mode "sqllogictest"
  "A mode for for working with 'sqllogictest' files."
  :syntax-table sqllogictest-mode-syntax-table
  (setq-local comment-start "#"
              comment-end "")

  (setq-local font-lock-defaults '(sqllogictest-mode-font-lock-keywords))
  (setq-local imenu-generic-expression sqllogictest-mode-imenu-generic-expression
              imenu-case-fold-search t)

  (run-hooks 'sqllogictest-mode-hook))

(add-to-list 'auto-mode-alist '("\\.slt\\'" . sqllogictest-mode))


;; Tex/Latex

(use-package tex-site
  :straight auctex
  :config
  (put 'LaTeX-narrow-to-environment 'disabled nil)

  ;; Use color instead of scaling section titles.
  (setq font-latex-fontify-sectioning 'color)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))


;; CSV

(use-package csv-mode
  :straight t
  :config
  (setq csv-align-padding 2)
  :hook ((csv-mode . csv-align-mode)))


;; Shell scripts

(use-package sh-script
  ;; built in
  )


;; Nix

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

;; Terraform/HCL

(use-package hcl-mode
  :straight t
  :mode "\\.tf\\|.tfvars\\'"
  :config
  (setf (alist-get 'hcl-mode apheleia-mode-alist) 'terraform)
  :hook ((hcl-mode . apheleia-mode)))


;; D2

(defvar d2-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Add '#' as a comment.
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\f ">" table)
    table)
  "Syntax table for `d2-mode'.")

(defvar d2-mode-font-lock-keywords
  '()
  "Keywords specification for `d2-mode'.")

(define-derived-mode d2-mode prog-mode "d2"
  "A mode for for working with 'd2' files."
  :syntax-table d2-mode-syntax-table
  (setq-local comment-start "#"
              comment-end "")

  (setq-local font-lock-defaults '(d2-mode-font-lock-keywords))

  (setq-local indent-tabs-mode nil
              tab-width 2
              indent-line-function 'insert-tab
              electric-indent-mode nil)

  (with-eval-after-load 'apheleia
    (setf (alist-get 'd2 apheleia-formatters) '("d2" "fmt" "-"))
    (setf (alist-get 'd2-mode apheleia-mode-alist) 'd2))

  (run-hooks 'd2-mode-hook))

(add-hook 'd2-mode-hook 'apheleia-mode)

(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))

;; Just

(use-package just-mode
  :straight t)

(provide 'seanmacs-langs)
;;; seanmacs-langs.el ends here
