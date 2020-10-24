;;; seanmacs-completions.el --- Completions -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion configuration.

;;; Code:

(use-package company
  :straight t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2)
  (setq company-backends (delete 'company-dabbrev company-backends))
  (setq company-frontends '(company-preview-frontend))
  (setq tab-always-indent 'complete)
  :hook ((after-init . global-company-mode))
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)))

(use-package flimenu
  :straight t
  :config
  (flimenu-global-mode))

(use-package selectrum
  :straight t
  :config
  (setq selectrum-count-style 'current/matches
        selectrum-fix-minibuffer-height t)
  ;; Disable since selectrum ordering doesn't match what emacs suggests.
  (setq suggest-key-bindings nil)
  (selectrum-mode +1))

(use-package prescient
  :straight t
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy)
        prescient-sort-length-enable nil))

(use-package selectrum-prescient
  :straight t
  :config
  (selectrum-prescient-mode))

(use-package imenu
  ;; built-in
  :config)

(use-package xref
  ;; built-in
  :config)

(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
        lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-prefer-capf t
        lsp-response-timeout 2))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-flycheck-enable t)
  :hook ((lsp-mode . lsp-ui-mode)))

(provide 'seanmacs-completions)
;;; seanmacs-completions.el ends here
