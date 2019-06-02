;;; packages.el --- Packages for go
;;; Code:

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
    (core/local 'go-mode
                "rn" 'go-rename
                "ta" 'go/go-tests-all
                "tv" 'go/go-tests-all-verbose
                "v" 'go/go-vendor)))

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

(provide '+go)
;;; packages.el ends here
