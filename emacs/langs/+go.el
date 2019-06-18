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
    (evil-collection-define-key 'normal 'go-mode-map (kbd "C-o") 'pop-tag-mark)
    (defvar core-go-mode-map (make-sparse-keymap))
    (define-key core-go-mode-map "rn" 'go-rename)
    (define-key core-go-mode-map "ta" 'go/go-tests-all)
    (define-key core-go-mode-map "tv" 'go/go-tests-all-verbose)
    (define-key core-go-mode-map "v" 'go/go-vendor)
    (add-hook 'go-mode-hook
              (lambda ()
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

(provide '+go)
;;; packages.el ends here
