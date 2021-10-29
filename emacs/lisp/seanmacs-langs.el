;;; seanmacs-langs.el --- Langs -*- lexical-binding: t; -*-

;;; Commentary:
;; Language configuration.

;;; Code:

;; Utilities for creating repls using comint.

(defface sm/comint-echo-face '((t :inherit shadow))
  "Face for text echoed in the comint buffer.")

(defun sm/comint-echo-and-send (proc str)
  "Send STR to PROC.  STR will be echoed after the prompt."
  (let ((lines (split-string str "\n" nil)))
    (with-current-buffer (process-buffer proc)
      (let ((echo (lambda (s)
                    (goto-char (process-mark proc))
                    (insert-before-markers (propertize (concat s "\n")
                                                       'font-lock-face 'sm/comint-echo-face))
                    (move-marker comint-last-input-end (point)))))
        (mapcar #'(lambda (line)
                    (funcall echo line)
                    (comint-send-string proc (concat line "\n")))
                lines)))))

(defun sm/comint-send-region (proc beg end)
  "Send the selected region BEG to END to some PROC."
  (unless (and beg end)
    (user-error "No region selected"))
  (let ((region (buffer-substring-no-properties beg end)))
    (sm/comint-echo-and-send proc (string-trim region))))

(defun sm/comint-send-line (proc)
  "Send the current line to PROC."
  (let ((str (thing-at-point 'line t)))
    (sm/comint-echo-and-send proc (string-trim str))))

(defun sm/comint-send-region-or-line (proc)
  "Send region if active, send the current line otherwise."
  (if (use-region-p)
      (progn
        (sm/comint-send-region proc (region-beginning) (region-end))
        (deactivate-mark))
    (sm/comint-send-line proc)))

(defun sm/comint-send-buffer (proc)
  "Send the contents of the current buffer to PROC."
  (sm/comint-send-region proc (point-min) (point-max)))

;; Keybind utilities for use in lang major modes.

(defun sm/set-goto-def-keybind (mode-map fn)
  "Add the appropriate goto def FN to MODE-MAP."
  (sm/set-jump-property fn)
  (evil-collection-define-key 'normal mode-map "gd" fn))

(defun sm/set-jump-property (fn)
  "Set jump property for FN."
  (evil-add-command-properties fn :jump t))

;; LSP utilities.

(defun sm/set-eglot-checker ()
  (setq flycheck-checker 'eglot)) ;; Buffer local.


;; Go

(use-package go-mode
  :straight t
  :defer t
  :commands gofmt
  :init
  (defun sm/set-gofmt-hook ()
    (add-hook 'before-save-hook #'gofmt nil t))

  (defun sm/gopls-ensure ()
    (setq eglot-workspace-configuration
          `((gopls . (directoryFilters ,(vector "-node_modules"
                                                "-vendor")))))
    (eglot-ensure))
  :config
  (setq gofmt-command "goimports"
        gofmt-args '("-local=coder.com,cdr.dev,go.coder.com,github.com/cdr"))
  (sm/set-goto-def-keybind 'go-mode-map #'xref-find-definitions)

  ;; Running golangci-lint can be slow. Enable on demand.
  (defun sm/enable-golangci ()
    "Enable golangci-lint flycheck checker."
    (interactive)
    (flycheck-golangci-lint-setup)
    (flycheck-add-next-checker 'eglot 'golangci-lint 'append))

  (defun sm/disable-golangci ()
    "Disable golangci-lint flycheck checker."
    (interactive)
    (flycheck-remove-next-checker 'eglot 'golangci-lint))

  (defun sm/go-toplevel-test ()
    "Get the name of the current top-level test function."
    (save-excursion
      (save-match-data
        (re-search-backward "func \\(Test[A-Za-z0-9_]+\\)(t \\*testing.T)\s+" nil t)
        (match-string 1))))

  (defun sm/go-run-test-at-point ()
    "Run top-level test at point."
    (interactive)
    (when-let ((test-name (sm/go-toplevel-test)))
      (let ((cmd (format "go test -run %s -v" test-name))
            (buf-name (format "*Test: Go [%s]*" test-name)))
        (sm/compile cmd buf-name))))

  :hook ((go-mode . sm/set-gofmt-hook)
         (go-mode . sm/set-eglot-checker)
         (go-mode . sm/gopls-ensure))
  :bind(:map go-mode-map
             ("C-c C-t" . sm/go-run-test-at-point)))

(use-package flycheck-golangci-lint
  :straight t
  :defer t
  :commands flycheck-golangci-lint-setup
  :config
  (setq flycheck-golangci-lint-tests t
        flycheck-golangci-lint-fast t))


;; Haskell

(use-package haskell-mode
  :straight t
  :defer t
  :config
  (setq haskell-stylish-on-save t
        haskell-mode-stylish-haskell-path "ormolu"
        haskell-interactive-popup-errors nil)
  (sm/set-goto-def-keybind 'haskell-mode-map #'haskell-mode-jump-to-def)
  :hook ((haskell-mode . interactive-haskell-mode)))


;; Octave

(use-package octave
  :defer t
  :mode ("\\.m\\'" . octave-mode))


;; Rust

(use-package rust-mode
  :straight t
  :defer t
  :config
  (setq rust-format-on-save t
        rust-format-show-buffer nil)

  (sm/add-server-program 'rust-mode "rust-analyzer")

  :hook ((rust-mode . cargo-minor-mode)
         (rust-mode . sm/set-eglot-checker)
         (rust-mode . eglot-ensure)))

(use-package cargo
  :straight t
  :defer t)


;; Typescript/ web stuff

(defun sm/bin-from-node-modules (bin)
  "Return the full path if BIN exists in dominating node modules
dir. Return nil otherwise."
  (let* ((dir (or (projectile-project-root)
                  (buffer-file-name)
                  default-directory))
         (root (locate-dominating-file dir "node_modules"))
         (rel-path (format "node_modules/.bin/%s" bin))
         (path (and root (expand-file-name rel-path root))))
    (when (and path (file-executable-p path))
      path)))

(defun sm/use-node-modules-eslint ()
  (when-let ((eslint (sm/bin-from-node-modules "eslint")))
    (setq-local flycheck-javascript-eslint-executable eslint)))

(use-package prettier-js
  :straight t
  :defer t
  :init
  (defun sm/use-node-modules-prettier ()
    (when-let ((prettier (sm/bin-from-node-modules "prettier")))
      (setq-local prettier-js-command prettier)))
  :hook ((prettier-js-mode . sm/use-node-modules-prettier)))

(use-package typescript-mode
  :straight t
  :defer t
  :mode "\\.ts\\'"
  :init
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)

  (defun sm/enable-ts-eslint ()
    "Enable javascript-esline flycheck checker for typescript."
    (interactive)
    (flycheck-add-next-checker 'eglot 'javascript-eslint))

  (defun sm/disable-ts-eslint ()
    "Disable javascript-eslint flycheck checker for typescript."
    (interactive)
    (flycheck-remove-next-checker 'eglot 'javascript-eslint))

  :config
  (sm/set-goto-def-keybind 'typescript-mode-map #'xref-find-definitions)

  :hook ((typescript-mode . eglot-ensure)
         (typescript-mode . prettier-js-mode)
         (typescript-mode . sm/use-node-modules-eslint)
         (typescript-mode . sm/set-eglot-checker))
  :bind (:map typescript-mode-map
              ("C-c C-d" . sm/eglot-lookup-doc)
              ("C-c r r" . eglot-rename)))

(define-derived-mode typescriptreact-mode typescript-mode "TSX"
  "Convenience mode for differentiating between regular
Typescript files and TSX files.

Note that `eglot' is able to automatically detect language ID
from the mode name. The typescript language server uses
'typescriptreact' as the language ID for tsx files, hence the
mode name."
  (flycheck-add-mode 'javascript-eslint 'typescriptreact-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescriptreact-mode))

(use-package web-mode
  :straight t
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :init
  (defun sm/reset-web-mode-offsets ()
    (dtrt-indent-adapt) ;; Only runs once per buffer, no harm in calling it here.
    (setq-local web-mode-code-indent-offset standard-indent)
    (setq-local web-mode-css-indent-offset standard-indent)
    (setq-local web-mode-markup-indent-offset standard-indent))
  :config
  (setq web-mode-comment-style 1
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-indentation nil)
  :hook ((web-mode . sm/reset-web-mode-offsets)))


;; Elixir

(use-package elixir-mode
  :straight t
  :defer t
  :init
  (defun sm/set-elixir-format-hook ()
    (add-hook 'before-save-hook #'elixir-format nil t))

  (defvar sm/iex-name "iex"
    "Command for iex.")

  (defvar sm/iex-buffer nil
    "Buffer where iex is running.")

  (define-derived-mode sm/iex-mode comint-mode "IEx"
    "Major mode for running iex."

    (set (make-local-variable 'comint-prompt-read-only) t)
    (set (make-local-variable 'comint-input-sender) 'sm/comint-echo-and-send))

  (defun sm/iex-start-process ()
    "Start a iex process with an associated buffer, and enable `sm/iex-mode'."
    (setq sm/iex-buffer (make-comint "IEx" sm/iex-name nil "-S" "mix"))
    (with-current-buffer sm/iex-buffer
      (sm/iex-mode)))

  (defun sm/iex-process ()
    "Return the process associated with the iex buffer.
Start a new process if not alive."
    (or (if (buffer-live-p sm/iex-buffer)
            (get-buffer-process sm/iex-buffer))
        (progn
          (sm/iex-start-process)
          (get-buffer-process sm/iex-buffer))))

  (defun sm/iex-send-region-or-line ()
    (interactive)
    (sm/comint-send-region-or-line (sm/iex-process)))

  (defun sm/iex-send-buffer ()
    (interactive)
    (sm/comint-send-buffer (sm/iex-process)))

  (defun sm/iex-recompile-project ()
    (interactive)
    (sm/comint-echo-and-send (sm/iex-process) "recompile"))

  (defun sm/elixir-current-module ()
    (save-excursion
      (save-match-data
        (re-search-backward "defmodule \\([A-Z][A-Za-z0-9\._]+\\)\s+" nil t)
        (match-string 1))))

  (defun sm/iex-recompile-current-module ()
    (interactive)
    (let* ((module (sm/elixir-current-module))
           (str (format "r %s" module)))
      (sm/comint-echo-and-send (sm/iex-process) str)))

  (defun sm/iex-alias-current-module ()
    (interactive)
    (let* ((module (sm/elixir-current-module))
           (str (format "alias %s" module)))
      (sm/comint-echo-and-send (sm/iex-process) str)))

  (defun sm/iex-project-run ()
    "Open an iex buffer for a project."
    (interactive)
    (when-let ((default-directory (projectile-project-root)))
      (pop-to-buffer (process-buffer (sm/iex-process)))))

  :config
  (sm/add-server-program 'elixir-mode "elixir-ls")

  (sm/set-goto-def-keybind 'elixir-mode-map #'xref-find-definitions)

  :hook ((elixir-mode . eglot-ensure)
         (elixir-mode . sm/set-elixir-format-hook)
         (elixir-mode . sm/set-eglot-checker))
  :bind(:map elixir-mode-map
             ("C-c C-l" . sm/iex-recompile-current-module)
             ("C-c C-c" . sm/iex-send-region-or-line)))


;; Erlang

(use-package erlang
  :straight t
  :defer t
  :config
  (setq erlang-electric-commands
        '(erlang-electric-comma
          erlang-electric-semicolon
          erlang-electric-gt
          erlang-electric-newline)))


;; Protobuf

(use-package protobuf-mode
  :straight t
  :defer t)


;; Common lisp

(use-package sly
  :straight t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"
        sly-contribs '(sly-fancy))
  (sm/set-jump-property #'sly-edit-definition))


;; Scheme

(use-package geiser
  :straight t
  :defer t
  :config
  (setq geiser-default-implementation 'guile))

(use-package geiser-guile
  :straight t
  :defer t)

(use-package geiser-racket
  :straight t
  :defer t)

(use-package geiser-chez
  :straight t
  :defer t)

(use-package geiser-mit
  :straight t
  :defer t)

(use-package geiser-chicken
  :straight t
  :defer t
  :init
  (setq flycheck-scheme-chicken-executable "chicken-csc"
        geiser-chicken-binary "chicken-csi"))


;; Julia

(use-package julia-mode
  :straight t
  :defer t
  :init

  (defvar sm/julia-name "julia"
    "Command to run julia repl.")

  (defvar sm/julia-opts '("--color=yes")
    "Additional options to pass to julia when starting repl.")

  (defvar sm/julia-buffer nil
    "Buffer where julia is running.")

  (defvar sm/julia-mode-hook nil
    "Hook for `sm/julia-mode'.")

  (defvar sm/julia-mode-map
    (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
      (define-key map (kbd "TAB") 'julia-latexsub-or-indent)
      map)
    "Mode map for `sm/julia-mode'.")

  (define-derived-mode sm/julia-mode comint-mode "Julia"
    "Major mode for running julia.

\\{sm/julia-mode-map}"
    (set (make-local-variable 'comint-prompt-read-only) t)
    (set (make-local-variable 'comint-input-sender) 'sm/comint-echo-and-send)

    ;; The default value of `electric-pair-default-skip-self' doesn't seem to
    ;; work well with this derived comint. I end up with an extra closing
    ;; parenthesis instead of getting a balanced pair when typing '()' verbatim.
    (set (make-local-variable 'electric-pair-skip-self) t)

    (run-hooks 'sm/julia-mode-hook))

  (defun sm/julia-start-process ()
    "Start a julia process and enable `sm/julia-mode'"
    (setq sm/julia-buffer (apply 'make-comint "Julia" sm/julia-name nil sm/julia-opts))
    (with-current-buffer sm/julia-buffer
      (sm/julia-mode)))

  (defun sm/julia-process ()
    "Return the process associated with the julia buffer.
Start a new process if not alive."
    (or (if (buffer-live-p sm/julia-buffer)
            (get-buffer-process sm/julia-buffer))
        (progn
          (sm/julia-start-process)
          (get-buffer-process sm/julia-buffer))))

  (defun sm/julia-send-region-or-line ()
    (interactive)
    (sm/comint-send-region-or-line (sm/julia-process)))

  (defun sm/julia-include-buffer ()
    (interactive)
    (if-let ((path (buffer-file-name)))
        (let* ((str (format "include(\"%s\")" (sm/path-localname path)))) ;; Don't include tramp info.
          (sm/comint-echo-and-send (sm/julia-process) str))))

  (defun sm/julia-end-of-defun ()
    "Move point to the end of the current function.
Return nil if point is not in a function, otherwise point.

This is the same as `julia-end-of-defun', except for an empty
line check to prevent stopping at blank lines."
    (interactive)
    (let ((beg-defun-indent))
      (when (or (julia-looking-at-beginning-of-defun)
                (julia-beginning-of-defun 1)
                (julia-beginning-of-defun -1))
        (beginning-of-line)
        (if (looking-at-p julia-function-assignment-regex)
            ;; f(x) = ...
            (progn
              ;; skip any dangling lines
              (while (and (forward-line)
                          (not (eobp))
                          (or (julia-indent-hanging)
                              ;; dangling closing paren
                              (and (eq 'paren (julia-syntax-context-type))
                                   (search-forward ")"))))))
          ;; otherwise skip forward to matching indentation (not in string/comment)
          (setq beg-defun-indent (current-indentation))
          (while (and (not (eobp))
                      (forward-line 1)
                      (or (forward-comment 5) ;; Skip empty lines.
                          (julia-syntax-comment-or-string-p)
                          (> (current-indentation) beg-defun-indent)))))
        (end-of-line)
        (point))))

  (defun sm/julia-send-function ()
    (interactive)
    (save-excursion
      (let ((beg (progn (julia-beginning-of-defun)
                        (point))))
        (set-mark beg)
        (sm/julia-end-of-defun)
        (goto-char (point))
        (sm/julia-send-region-or-line))))

  (defun sm/region-string ()
    "Get region string, or nil if no region is selected."
    (when (use-region-p)
      (buffer-substring (region-beginning) (region-end))))

  (defun sm/julia-doc ()
    (interactive)
    (let* ((sym (or (sm/region-string) (thing-at-point 'symbol t)))
           (str (concat "@doc " sym)))
      (sm/comint-echo-and-send (sm/julia-process) str)))

  (defun sm/julia-edit ()
    (interactive)
    ;; TODO: Should be getting expression, but line is close enough for now.
    (let* ((line (or (sm/region-string) (thing-at-point 'line t)))
           (str (concat "@edit " line)))
      (sm/comint-echo-and-send (sm/julia-process) str)))

  (defun sm/julia-methods (sym)
    "Get methods for a function symbol."
    (interactive (list (or (sm/region-string) (thing-at-point 'symbol t))))
    (sm/comint-echo-and-send (sm/julia-process) (format "methods(%s)" sym)))

  (defun sm/julia-macroexpand ()
    "Expand macro on the current line, or region if selected."
    (interactive)
    ;; As with `sm/julia-edit', "should" be an expression instead.
    (let ((thing (or (sm/region-string) (thing-at-point 'line t))))
      (sm/comint-echo-and-send (sm/julia-process) (concat "@macroexpand " thing))))

  (defun sm/julia-apropos (search)
    "Send apropos query to repl."
    (interactive (list (read-string "Apropos: ")))
    (let ((str (format "apropos(\"%s\")" search)))
      (sm/comint-echo-and-send (sm/julia-process) str)))

  (defun sm/julia-project-root ()
    "Return project root if inside Julia project, NIL otherwise."
    (locate-dominating-file "." "Project.toml"))

  (defun sm/julia-activate-project ()
    "Activate and instantiate the julia project if available."
    (interactive)
    (when-let ((project-root (sm/julia-project-root)))
      (let* ((path (expand-file-name (concat project-root "Project.toml")))
             (cmd (format "using Pkg; Pkg.activate(\"%s\"); Pkg.instantiate()"
                          (sm/path-localname path))))
        (sm/comint-echo-and-send (sm/julia-process) cmd))))

  (defun sm/julia-cd (dir)
    "Change working directory for repl to DIR."
    (interactive "D")
    (let* ((path (expand-file-name (sm/path-localname dir))) ;; Don't include tramp info.
           (str (format "cd(\"%s\")" path)))
      ;; Keep default directory in sync with where the repl points.
      (with-current-buffer (process-buffer (sm/julia-process))
        (cd dir))
      (sm/comint-echo-and-send (sm/julia-process) str)))

  (defun sm/julia-run ()
    "Open a julia buffer.

Attempt to start the repl in the project root if available.
Otherwise start the repl in the current directory."
    (interactive)
    (let ((default-directory (or (sm/julia-project-root)
                                 (projectile-project-root)
                                 default-directory)))
      (pop-to-buffer (process-buffer (sm/julia-process)))))

  :bind(:map sm/julia-mode-map
             ("C-c C-k" . comint-send-eof)
             ("C-c C-d d" . sm/julia-doc)
             ("C-c C-d C-d" . sm/julia-doc)
             ("C-c C-d a" . sm/julia-apropos)
             ("C-c C-d C-a" . sm/julia-apropos)
             ("C-c C-d m" . sm/julia-methods)
             ("C-c C-d C-m" . sm/julia-methods)
             :map julia-mode-map
             ("C-c C-z" . sm/julia-run)
             ("C-c C-a" . sm/julia-activate-project)
             ("C-c C-l" . sm/julia-include-buffer)
             ("C-c C-c" . sm/julia-send-region-or-line)
             ("C-c C-p" . sm/julia-send-function)
             ("C-c C-d d" . sm/julia-doc)
             ("C-c C-d C-d" . sm/julia-doc)
             ("C-c C-d a" . sm/julia-apropos)
             ("C-c C-d C-a" . sm/julia-apropos)
             ("C-c C-m" . sm/julia-macroexpand)
             ("C-c C-d m" . sm/julia-methods)
             ("C-c C-d C-m" . sm/julia-methods)))


;; R

(use-package ess
  :straight t
  :defer t
  :init
  (defun sm/ess-r-comint-vars ()
    (setq-local comint-scroll-to-bottom-on-output t)
    (setq-local comint-prompt-read-only t))
  :config
  (setq ess-use-ido nil)
  ;; Make help buffer more evil like.
  (evil-set-initial-state 'ess-r-help-mode 'normal)
  (evil-collection-define-key 'normal 'ess-help-mode-map
    (kbd "q") 'kill-current-buffer
    (kbd "]]") 'ess-skip-to-next-section
    (kbd "[[") 'ess-skip-to-previous-section)
  :hook ((inferior-ess-r-mode . sm/ess-r-comint-vars)))


;; Clojure

(use-package clojure-mode
  :straight t
  :defer t
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)))

(use-package cider
  :straight t
  :defer t
  :config
  (sm/set-jump-property #'cider-find-var))


;; Emacs lisp

(use-package elisp-mode
  ;; built-in
  :defer t
  :init
  (defun seanmacs/elisp-describe-symbol-at-point ()
    (interactive)
    (let ((sym (symbol-at-point)))
      (describe-symbol sym)))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-d" . seanmacs/elisp-describe-symbol-at-point)
              :map lisp-interaction-mode-map
              ("C-c C-d" . seanmacs/elisp-describe-symbol-at-point)))


;; Yaml

(use-package yaml-mode
  :straight t
  :defer t)


;; Markdown

(use-package markdown-mode
  :straight t
  :defer t
  :config
  (setq-default markdown-hide-urls t)
  (setq markdown-url-compose-char ?#
        markdown-fontify-code-blocks-natively t))


;; Dockerfile

(use-package dockerfile-mode
  :straight t
  :defer t)


;; Nix

(use-package nix-mode
  :straight t
  :defer t)


;; Lean

(use-package lean-mode
  :straight t
  :defer t
  :config
  (setq lean-memory-limit 16384
        lean-extra-arguments '("-D class.instance_max_depth=1000"))
  (sm/set-goto-def-keybind 'lean-mode-map #'lean-find-definition))


;; Purescript

(defvar seanmacs/purescript-formatter "purty")

(use-package purescript-mode
  :straight t
  :defer t
  :init
  (reformatter-define purescript-format
    :program seanmacs/purescript-formatter
    :args '("-"))
  :config
  (sm/set-goto-def-keybind 'purescript-mode-map #'psc-ide-goto-definition)
  :bind(:map purescript-mode-map
             ("C-c C-r r" . psci)
             ("C-c C-r l" . psci/load-current-file!)
             ("C-c C-r m" . psci/load-module!))
  :hook ((purescript-mode . turn-on-purescript-indentation)
         (purescript-mode . psc-ide-mode)))

(use-package psc-ide
  :straight t
  :defer t)

(use-package psci
  :straight t
  :defer t
  :commands (psci psci/load-module! psci/load-current-file!))


;; Agda

(use-package agda2-mode
  :straight t
  :defer t
  :init
  (defun sm/current-agda-version ()
    "Return the version string for Agda."
    (when-let ((exe (executable-find "agda")))
      (let ((out (shell-command-to-string (format "%s --version" exe))))
        ;; Assumes "Agda version <version>"
        (nth 2 (split-string out)))))

  (defun sm/set-agda-version ()
    "Set `agda2-version' to the version of the currently installed Agda bin."
    (interactive)
    (when-let ((ver (sm/current-agda-version)))
      (message "Setting `agda2-version' to %s" ver)
      (setq agda2-version ver)))

  (defun sm/set-input-agda ()
    (require 'agda-input)
    (set-input-method "Agda"))
  :config
  (sm/set-goto-def-keybind 'agda2-mode-map #'agda2-goto-definition-keyboard)

  (setq agda2-highlight-face-groups 'default-faces)
  :hook ((agda2-mode . sm/set-input-agda)
         (agda2-mode . whitespace-turn-off)))


;; Plantuml

(use-package plantuml-mode
  :straight t
  :defer t
  :config
  (setq plantuml-default-exec-mode 'executable))


;; TLA+

(define-derived-mode tlaplus-mode prog-mode "TLA+"
  "Mode for working with TLA+ files."
  (setq-local comment-start "(*"
              comment-end "*)"))

(add-to-list 'auto-mode-alist '("\\.tla\\'" . tlaplus-mode))

;; Nice to have things updated immediately when running 'pcal'.
(add-hook 'tlaplus-mode-hook #'auto-revert-mode)


;; C/C++

(use-package cc-mode
  ;; built-in
  :config
  (sm/add-server-program 'c++-mode "ccls")
  (sm/add-server-program 'c-mode "ccls")
  :hook ((c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)))

(provide 'seanmacs-langs)
;;; seanmacs-langs.el ends here
