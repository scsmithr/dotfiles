;;; packages.el --- Packages for typescript

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
        web-mode-enable-auto-indentation nil
            )
      (add-hook 'web-mode-hook
                (lambda ()
                  (when (string-equal "tsx" (file-name-extension buffer-file-name))
            (setup-tide-mode))))
      ;; enable typescript-tslint checker
      (add-hook 'web-mode-hook #'lsp)
      (flycheck-add-mode 'typescript-tslint 'web-mode)))

(provide '+typescript)
;;; packages.el ends here
