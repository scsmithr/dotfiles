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

  (unbind-key [return] company-active-map)
  (unbind-key (kbd "RET") company-active-map)
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
  (setq selectrum-fix-vertical-window-height t
        selectrum-count-style 'current/matches)
  ;; Disable since selectrum ordering doesn't match what emacs suggests.
  (setq suggest-key-bindings nil)
  (selectrum-mode +1))

(use-package prescient
  :straight t
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy)
        prescient-sort-length-enable nil
        prescient-sort-full-matches-first t)
  (prescient-persist-mode))

(use-package selectrum-prescient
  :straight t
  :config
  (selectrum-prescient-mode))

(use-package imenu
  ;; built-in
  :bind (("C-c b s" . imenu)))

(use-package xref
  ;; built-in
  :bind (("C-c x a" . xref-find-apropos)
         ("C-c x r" . xref-find-references)))

(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil
        lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-prefer-capf t
        lsp-response-timeout 2
        lsp-modeline-code-actions-segments '(count)
        lsp-file-watch-threshold nil
        lsp-headerline-breadcrumb-enable nil
        lsp-progress-prefix "*")

  ;; Ignore next
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.next\\'"))

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
