;;; seanmacs-completions.el --- Completions -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion configuration.

;;; Code:

(use-package company
  :straight t
  :config
  (setq company-frontends '(company-posframe-frontend)
        company-minimum-prefix-length 1
        company-idle-delay 0.2)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (setq company-backends (delete 'company-dabbrev company-backends))
  (shackle '(("^\\*company-documentation\\*" :height 0.3)))
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-posframe
  :straight t
  :init
  (setq company-posframe-show-indicator nil
        company-posframe-show-metadata nil
        company-posframe-quickhelp-delay nil))

(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
        lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-prefer-capf t
        lsp-response-timeout 2)
  (shackle '(("^\\*lsp-help\\*" :height 0.3))))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-flycheck-enable t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(provide 'seanmacs-completions)
;;; seanmacs-completions.el ends here
