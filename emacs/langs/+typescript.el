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

(provide '+typescript)
;;; packages.el ends here
