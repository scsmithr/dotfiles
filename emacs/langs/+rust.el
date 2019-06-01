;;; packages.el --- Packages for rust

(defun rust/init-rust-mode ()
    (use-package rust-mode
      :ensure t
      :defer t
      :config
      (setq rust-format-on-save t)
      ;; See https://github.com/tigersoldier/company-lsp/issues/61
      (add-hook 'rust-mode-hook
          (lambda () (setq company-backends
                           (delete 'company-capf company-backends))))
      (add-hook 'rust-mode-hook #'lsp)))

(provide '+rust)
;;; packages.el ends here
