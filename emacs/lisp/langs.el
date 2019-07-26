;;; langs.el --- Language stuff
;;; Code:

;; Go

(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (evil-add-command-properties #'godef-jump :jump t)
  (setq gofmt-command "goimports"))
(use-package go-rename
  :ensure t)

(core/local 'go-mode-map
            "rn" 'go-rename
            "ta" 'go/go-tests-all
            "tv" 'go/go-tests-all-verbose
            "v" 'go/go-vendor)

(add-hook 'go-mode-hook
          (lambda ()
            (setq company-backends (delete 'company-capf company-backends))))

(defvar go-test-buffer-name "*go test*"
  "Name of buffer for go test output.")

(defvar go-vendor-buffer-name "*go vendor*"
  "Name of buffer for go test output.")

(defun go/go-tests (args)
  (interactive)
  (compilation-start (concat "cd " (projectile-project-root)
                             " && " "go test " args)
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
                             " && GO111MODULE=on go mod vendor")
                     nil (lambda (n) go-vendor-buffer-name) nil))

;; Haskell

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (setq haskell-stylish-on-save t)
  (setq haskell-mode-stylish-haskell-path "brittany"))

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
  :ensure t
  :defer t
  :config
  (setq rust-format-on-save t)
  (setq lsp-rust-clippy-preference "on")
  ;; See https://github.com/tigersoldier/company-lsp/issues/61
  (add-hook 'rust-mode-hook
            (lambda () (setq company-backends
                             (delete 'company-capf company-backends))))
  (add-hook 'rust-mode-hook #'lsp))

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
  :init
  :ensure t
  :after (web-mode company flycheck))

(use-package prettier-js
  :ensure t
  :after (web-mode))

(use-package web-mode
  :ensure t
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
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :weight 'bold
                      :background (doom-transparentize 'cyan 0.5))
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
  :ensure t
  :defer t
  :config
  ;; Defaults to dark blue with doom emacs theme. Doom solarized light seems
  ;; to have it set to some default color, isn't easy to read.
  (set-face-attribute 'elixir-atom-face nil :foreground (doom-color 'blue))
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (evil-add-command-properties #'alchemist-goto-defintion-at-point :jump t))

(use-package alchemist
  :ensure t
  :defer t)

(core/local 'elixir-mode-map
            "mc" 'alchemist-mix-compile
            "mr" 'alchemist-mix-run)

;; Markdown

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-face-attribute markdown-header-delimiter-face nil
                                :foreground (doom-color 'fg-alt))
            (set-face-attribute markdown-header-face-1 nil
                                :inherit 'outline-1)
            (set-face-attribute markdown-header-face-2 nil
                                :inherit 'outline-2)
            (set-face-attribute markdown-header-face-3 nil
                                :inherit 'outline-3)
            (set-face-attribute markdown-header-face-4 nil
                                :inherit 'outline-4)
            (set-face-attribute markdown-header-face-5 nil
                                :inherit 'outline-5)
            (set-face-attribute markdown-header-face-6 nil
                                :inherit 'outline-6)
            (flyspell-mode 1)))

(provide 'langs)
;;; langs.el ends here
