;;; packages.el --- Packages for rust

(defun rust/init-rust-mode ()
    (use-package rust-mode
      :ensure t
      :defer t
      :config
      (setq rust-format-on-save t)
      (add-hook 'rust-mode-hook #'lsp)))
