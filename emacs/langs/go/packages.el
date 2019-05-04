;;; packages.el --- Packages for go
;;; Code:

(defun go/init-go-mode ()
  "Initialize go related features."
  (use-package go-mode
    :ensure t
    :defer t
    :config
    (setq gofmt-command "goimports")
    (evil-collection-define-key 'normal 'go-mode-map
      "K" 'godoc-at-point)
    :init
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook #'lsp)))

(provide 'go)
;;; packages.el ends here
