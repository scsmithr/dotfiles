;;; langs.el --- Language stuff
;;; Code:

;; Go

(defun go/init-go-mode ()
  "Initialize go related features."
  (progn
    (use-package go-mode
      :ensure t
      :defer t
      :config
      (add-hook 'go-mode-hook #'lsp)
      (add-hook 'before-save-hook #'gofmt-before-save)
      (setq gofmt-command "goimports"))
    (use-package go-rename
      :ensure t)
    (evil-add-command-properties #'godef-jump :jump t)
    (defvar core-go-mode-map (make-sparse-keymap))
    (define-key core-go-mode-map "rn" 'go-rename)
    (define-key core-go-mode-map "ta" 'go/go-tests-all)
    (define-key core-go-mode-map "tv" 'go/go-tests-all-verbose)
    (define-key core-go-mode-map "v" 'go/go-vendor)
    (add-hook 'go-mode-hook
              (lambda ()
                (setq company-backends (delete 'company-capf company-backends))
                (core/leader core-local-leader-key core-go-mode-map)))))

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

(defun haskell/init-haskell-mode ()
    (use-package haskell-mode
      :ensure t
      :defer t
      :config
      (setq haskell-stylish-on-save t)
      (setq haskell-mode-stylish-haskell-path "brittany")))

;; Octave

(defun octave/init-octave-mode ()
  (progn
    (use-package octave
      :defer t
      :mode ("\\.m\\'" . octave-mode))
    (defvar core-octave-mode-map (make-sparse-keymap))
    (define-key core-octave-mode-map "o" 'run-octave)
    (define-key core-octave-mode-map "sr" 'octave-send-region)
    (define-key core-octave-mode-map "sb" 'octave-send-buffer)
    (define-key core-octave-mode-map "sl" 'octave-send-line)
    (add-hook 'octave-mode-hook
              (lambda()
                (core/leader core-local-leader-key core-octave-mode-map)))))

;; Rust

(defun rust/init-rust-mode ()
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
      (add-hook 'rust-mode-hook #'lsp)))

;; Typescript

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(defun typescript/init-tide-mode ()
    (use-package tide
      :init
      :ensure t
      :after (web-mode company flycheck)))

(defun typescript/init-prettier ()
  (use-package prettier-js
    :ensure t
    :after (web-mode)))

(defun typescript/init-web-mode ()
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
            web-mode-comment-style 4

            web-mode-enable-css-colorization t
            web-mode-enable-auto-pairing t
            web-mode-enable-comment-keywords t
            web-mode-enable-current-element-highlight t
            web-mode-enable-auto-indentation nil)
      (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :weight 'bold
                      :background (doom-transparentize 'cyan 0.5))
      (add-hook 'web-mode-hook
              (lambda ()
                (when (string-match-p "tsx?" (file-name-extension buffer-file-name))
                  (setup-tide-mode)
                  (evil-add-command-properties #'tide-jump-to-definition :jump t)
                  (prettier-js-mode)
                  (flycheck-add-mode 'javascript-eslint 'web-mode))))))

(provide 'langs)
;;; langs.el ends here
