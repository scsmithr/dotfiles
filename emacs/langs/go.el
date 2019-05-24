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
    (define-key leader-map "t" 'go/go-tests-all)
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook #'lsp)))

(defvar go-test-verbose nil
  "Test verbosity.")

(defvar go-test-buffer-name "*go test*"
  "Name of buffer for go test output.")

(defun go/go-tests (args)
  (interactive)
  (compilation-start (concat "cd " (projectile-project-root)
                             " && " "go test " (when go-test-verbose "-v ") args)
                     nil (lambda (n) go-test-buffer-name) nil))

(defun go/go-tests-all ()
  (interactive)
  (go/go-tests "./..."))

(provide 'go)
;;; packages.el ends here
