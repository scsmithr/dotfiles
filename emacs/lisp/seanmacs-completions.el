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

  ;; Occasionally lsp might send a completion that company fails to handle due
  ;; to indexing. These errors don't impact anything, so limit how much
  ;; attention they receive.
  (advice-add 'company-preview-show-at-point :around #'ignore-errors-fn)

  :hook ((after-init . global-company-mode))
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)))

(use-package flimenu
  :straight t
  :config
  (defface sm/flimenu-prefix-face '((t :inherit font-lock-keyword-face))
    "Face for prefixes in imenu entries.")

  (defun sm/flimenu-format-entry (entry-name &optional prefixes)
    (if prefixes
        (let ((prop-prefix (propertize
                            (concat "[" (string-join prefixes "/") "]")
                            'face 'sm/flimenu-prefix-face)))
          (string-join (list prop-prefix entry-name) " "))
      entry-name))

  (setq flimenu-imenu-separator #'sm/flimenu-format-entry)

  (flimenu-global-mode))

(use-package imenu
  ;; built-in
  :config
  (setq imenu-auto-rescan t
        imenu-space-replacement " ")
  :hook ((imenu-after-jump . sm/recenter))
  :bind (("C-c b s" . imenu)))

(use-package selectrum
  :straight t
  :config
  (setq selectrum-fix-vertical-window-height t
        selectrum-count-style 'current/matches
        selectrum-max-window-height 10)
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

(use-package xref
  ;; built-in
  :config
  (defun sm/xref-show ()
    "Show xref result under point, keeping cursor in the xref window."
    (interactive)
    (sm/save-window
     (xref-goto-xref)))

  (evil-collection-define-key 'normal 'xref--xref-buffer-mode-map
    (kbd "SPC") #'sm/xref-show)

  :bind (("C-c x a" . xref-find-apropos)
         ("C-c x r" . xref-find-references)))

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (defvar sm/lsp-truncate-messages t
    "Should messages printed by lsp be truncated.")

  (defun sm/lsp-wrap-message (orig-fn &rest args)
    (let ((message-truncate-lines sm/lsp-truncate-messages))
      (apply orig-fn args)))

  (advice-add 'lsp--message :around #'sm/lsp-wrap-message)

  :config
  (sm/warn-fn-not-bound 'lsp--message)

  (setq lsp-prefer-flymake nil
        lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-prefer-capf t
        lsp-response-timeout 2
        lsp-modeline-code-actions-segments '(count)
        lsp-headerline-breadcrumb-enable nil
        lsp-progress-prefix "*** ")

  ;; Not sure that I want this. Will need to come up with better colors if I do.
  (setq lsp-diagnostics-attributes nil)

  ;; Never warn me about attempting to watch a large directory, but also disable
  ;; watching altogether. The regexps for ignoring watched directories is a bit
  ;; hard to control for larger repos with submodules. Also, gopls *will* spam
  ;; when directories change messages causing emacs to grind to a halt,
  ;; particularly on git checkouts. I would rather have checkouts be fast, and
  ;; call `lsp-workspace-restart' if needed.
  (setq lsp-file-watch-threshold nil
        lsp-enable-file-watchers nil)

  ;; These changes only matter if file watching is enabled.
  ;; Ignore next
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.next\\'")
  ;; Ignore go module vendor
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'")

  ;; Additional lsp clients (mostly for working with tramp).
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda () (cons lsp-go-gopls-server-path lsp-go-gopls-server-args)))
                    :major-modes '(go-mode go-dot-mod-mode)
                    :language-id "go"
                    :priority 0
                    :server-id 'gopls-remote
                    :completion-in-comments? t
                    :remote? t
                    :library-folders-fn #'lsp-go--library-default-directories
                    :after-open-fn (lambda ()
                                     ;; https://github.com/golang/tools/commit/b2d8b0336
                                     (setq-local lsp-completion-filter-on-incomplete nil))))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection (lambda ()
                                                            `(,(lsp-package-path 'typescript-language-server)
                                                              "--tsserver-path"
                                                              ,(lsp-package-path 'typescript)
                                                              ,@lsp-clients-typescript-server-args)))
                    :activation-fn 'lsp-typescript-javascript-tsx-jsx-activate-p
                    :priority -2
                    :completion-in-comments? t
                    :remote? t
                    :initialization-options (lambda ()
                                              (list :plugins lsp-clients-typescript-plugins
                                                    :logVerbosity lsp-clients-typescript-log-verbosity
                                                    :tsServerPath (lsp-package-path 'typescript)))
                    :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                    :server-id 'ts-ls-remote
                    :request-handlers (ht ("_typescript.rename" #'lsp-javascript--rename)))))

(provide 'seanmacs-completions)
;;; seanmacs-completions.el ends here
