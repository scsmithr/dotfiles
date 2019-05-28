;;; packages.el --- Packages for go
;;; Code:

(defun go/init-go-mode ()
  "Initialize go related features."
  (use-package go-mode
    :ensure t
    :defer t
    :config
    (setq gofmt-command "goimports")
    :init
    (add-hook 'go-mode-hook
              (lambda ()
              (defvar mode-map (make-sparse-keymap))
              (define-key leader-map "m" mode-map)
              (define-key mode-map "t" 'go/go-tests-all)
              (define-key mode-map "T" 'go/go-tests-all-verbose)
              (define-key mode-map "v" 'go/go-vendor)))
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook #'lsp)))

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

(provide 'go)
;;; packages.el ends here
