;;; seanmacs-langs.el --- Langs -*- lexical-binding: t; -*-

;;; Commentary:
;; Language configuration.

;;; Code:

;; Utilities for creating repls using comint.

(defun sm/comint-print-and-send (proc str)
  "Send STR to PROC.  STR will be echoed after the prompt."
  (let ((lines (split-string str "\n" nil)))
    (with-current-buffer (process-buffer proc)
      (mapcar (lambda (line)
                (goto-char (process-mark proc))
                (insert-before-markers (concat line "\n"))
                (move-marker comint-last-input-end (point))
                (comint-send-string proc (concat line "\n")))
              lines))))

(defun sm/comint-send-region (proc beg end)
  "Send the selected region BEG to END to some PROC."
  (unless (and beg end)
    (user-error "No region selected"))
  (let ((region (buffer-substring-no-properties beg end)))
    (sm/comint-print-and-send proc region)))

(defun sm/comint-send-line (proc)
  "Send the current line to PROC."
  (let ((str (thing-at-point 'line t)))
    (sm/comint-print-and-send proc (string-trim str))))

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

;; Go

(use-package go-mode
  :straight t
  :defer t
  :commands gofmt
  :init
  (defun seanmacs/gofmt-before-save ()
    (when (eq major-mode 'go-mode)
      (gofmt)))
  :config
  (setq gofmt-command "goimports"
        gofmt-args '("-local=go.coder.com"))
  (sm/set-goto-def-keybind 'go-mode-map #'lsp-find-definition)

  ;; Running golangci-lint can be slow. Enable on demand.
  (defun sm/enable-golangci ()
    "Enable golangci-lint flycheck checker."
    (interactive)
    (flycheck-golangci-lint-setup)
    (flycheck-add-next-checker 'lsp 'golangci-lint 'append))

  (defun sm/disable-golangci ()
    "Disable golangci-lint flycheck checker."
    (interactive)
    (flycheck-remove-next-checker 'lsp 'golangci-lint))

  :hook ((go-mode . lsp)
         (before-save . seanmacs/gofmt-before-save))
  :bind(:map go-mode-map
             ("C-c C-d" . lsp-describe-thing-at-point)
             ("C-c r r" . lsp-rename)))

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
        haskell-mode-stylish-haskell-path "brittany")
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
        rust-format-show-buffer nil
        lsp-rust-clippy-preference "on")
  :hook ((rust-mode . lsp)
         (rust-mode . cargo-minor-mode))
  :bind(:map rust-mode-map
             ("C-c C-d" . lsp-describe-thing-at-point)
             ("C-c r r" . lsp-rename)))

(use-package cargo
  :straight t
  :defer t)

;; Typescript

(defun seanmacs/bin-from-node-modules (bin)
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

(use-package tide
  :straight t
  :defer t
  :commands tide-setup)

(use-package prettier-js
  :straight t
  :defer t
  :init
  (defun seanmacs/use-node-modules-prettier ()
    (when-let ((prettier (seanmacs/bin-from-node-modules "prettier")))
      (setq-local prettier-js-command prettier)))
  :hook ((prettier-js-mode . seanmacs/use-node-modules-prettier)))

(use-package web-mode
  :straight t
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :init
  (defun sm/use-node-modules-eslint ()
    (when-let ((eslint (seanmacs/bin-from-node-modules "eslint")))
      (setq-local flycheck-javascript-eslint-executable eslint)))

  (defun sm/setup-tide ()
    (when (string-match-p "tsx?" (file-name-extension buffer-file-name))
      (tide-setup)
      (tide-hl-identifier-mode +1)
      (prettier-js-mode)
      (sm/set-jump-property #'tide-jump-to-definition)
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)))

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
  :hook ((web-mode . sm/setup-tide)
         (web-mode . sm/use-node-modules-eslint)
         (web-mode . sm/reset-web-mode-offsets))
  :bind (:map web-mode-map
              ("C-c C-d" . tide-documentation-at-point)
              ("C-c r r" . tide-rename-symbol)
              ("C-c r f" . tide-rename-file)))

;; Elixir

(use-package elixir-mode
  :straight t
  :defer t
  :init
  (defun sm/elixir-format-on-save ()
    (when (eq major-mode 'elixir-mode)
      (elixir-format)))

  (defvar sm/iex-name "iex"
    "Command for iex.")

  (defvar sm/iex-prompt-regex "^\\(iex\\|\\.\\.\\.\\)(.+)>"
    "Regex to match iex prompt.")

  (defvar sm/iex-buffer nil
    "Buffer where iex is running.")

  (define-derived-mode sm/iex-mode comint-mode "IEx"
    "Major mode for running iex."

    (set (make-local-variable 'comint-prompt-regexp) sm/iex-prompt-regex)
    (set (make-local-variable 'comint-prompt-read-only) t)
    (set (make-local-variable 'comint-input-sender) 'sm/comint-print-and-send))

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

  (defun sm/iex-project-run ()
    "Open an iex buffer for a project."
    (interactive)
    (when-let ((default-directory (projectile-project-root)))
      (pop-to-buffer (process-buffer (sm/iex-process)))))

  :config
  (setq lsp-clients-elixir-server-executable "elixir-ls")

  (sm/set-goto-def-keybind 'elixir-mode-map #'lsp-find-definition)

  :hook ((elixir-mode . lsp)
         (before-save . sm/elixir-format-on-save))
  :bind(:map elixir-mode-map
             ("C-c C-d" . lsp-describe-thing-at-point)
             ("C-c r r" . lsp-rename)
             ("C-c C-l" . sm/iex-send-buffer)
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

(use-package slime
  :straight t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy)))

;; Scheme

(use-package geiser
  :straight t
  :defer t)

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

  (define-derived-mode sm/julia-mode comint-mode "Julia"
    "Major mode for running julia."

    (set (make-local-variable 'comint-prompt-read-only) t)
    (set (make-local-variable 'comint-input-sender) 'sm/comint-print-and-send)

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

  (defun sm/julia-send-buffer ()
    (interactive)
    (sm/comint-send-buffer (sm/julia-process)))

  (defun sm/julia-send-function ()
    (interactive)
    (save-excursion
      ;; TODO: Handle empty lines in function body.
      (mark-defun)
      (sm/julia-send-region-or-line)))

  (defun sm/julia-doc ()
    (interactive)
    (let* ((sym (thing-at-point 'symbol t))
           (str (concat "@doc " sym)))
      (sm/comint-print-and-send (sm/julia-process) str)))

  (defun sm/julia-edit ()
    (interactive)
    ;; TODO: Should be getting expression, but line is close enough for now.
    (let* ((line (thing-at-point 'line t))
           (str (concat "@edit " line)))
      (sm/comint-print-and-send (sm/julia-process) str)))

  (defun sm/julia-apropos (search)
    "Send apropos query to repl."
    (interactive (list (read-string "Apropos: ")))
    (let ((str (format "apropos(\"%s\")" search)))
      (sm/comint-print-and-send (sm/julia-process) str)))

  (defun sm/julia-activate-project ()
    "Activate the julia project if available."
    (interactive)
    (let* ((project-file "Project.toml")
           (dir (locate-dominating-file "." project-file)))
      (if dir
          (let* ((path (expand-file-name (concat dir project-file)))
                 (cmd (format "using Pkg; Pkg.activate(\"%s\")" path)))
            (sm/comint-print-and-send (sm/julia-process) cmd))
        (message "Not in Julia project"))))

  (defun sm/julia-run ()
    "Open an julia buffer."
    (interactive)
    (let ((default-directory (or (projectile-project-root)
                                 default-directory)))
      (pop-to-buffer (process-buffer (sm/julia-process)))))

  :bind(:map sm/julia-mode-map
             ("C-c C-k" . comint-send-eof)
             ("C-c C-d d" . sm/julia-doc)
             ("C-c C-d C-d" . sm/julia-doc)
             ("C-c C-d a" . sm/julia-apropos)
             ("C-c C-d C-a" . sm/julia-apropos)
             :map julia-mode-map
             ("C-c C-a" . sm/julia-activate-project)
             ("C-c C-l" . sm/julia-send-buffer)
             ("C-c C-c" . sm/julia-send-region-or-line)
             ("C-c C-p" . sm/julia-send-function)
             ("C-c C-d d" . sm/julia-doc)
             ("C-c C-d C-d" . sm/julia-doc)
             ("C-c C-d a" . sm/julia-apropos)
             ("C-c C-d C-a" . sm/julia-apropos)))

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
  :straight t
  :defer t)

;; Markdown

(use-package markdown-mode
  :straight t
  :defer t
  :config
  (evil-collection-define-key 'normal 'markdown-mode-map
    (kbd "TAB") 'markdown-cycle))

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

(provide 'seanmacs-langs)
;;; seanmacs-langs.el ends here
