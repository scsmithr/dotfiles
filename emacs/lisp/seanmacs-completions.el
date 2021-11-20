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
  :hook ((imenu-after-jump . recenter))
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
    (sm/save-window-excursion
     (xref-goto-xref)))

  (evil-collection-define-key 'normal 'xref--xref-buffer-mode-map
    (kbd "SPC") #'sm/xref-show))

(use-package eldoc
  ;; built-in, ish. Eglot pulls in development versions.
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package eglot
  :straight t
  :defer t
  :commands (eglot eglot-ensure sm/add-server-program)
  :config
  ;; Seem to hit this with gopls and vendored deps.
  (setq max-specpdl-size 16000
        max-lisp-eval-depth 3200)

  (setq eglot-send-changes-idle-time 1)

  ;; Mostly done to ensure eglot is loaded before attempting to modify the
  ;; server programs list.
  (defun sm/add-server-program (mode program)
    "Helper for adding lsp server programs for MODE."
    (add-to-list 'eglot-server-programs `(,mode . (,program))))

  (defvar sm/eglot-help-buffer nil)

  (mapc #'sm/warn-fn-not-bound '(eglot--dbind
                                 eglot--current-server-or-lose
                                 eglot--TextDocumentIdentifier
                                 eglot--hover-info))

  ;; Adapted from Doom Emacs.
  (defun sm/eglot-lookup-doc ()
    "Request documentation for the thing at point."
    (interactive)
    (eglot--dbind ((Hover) contents range)
        (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                         (eglot--TextDocumentPositionParams))
      (let ((blurb (and (not (seq-empty-p contents))
                        (eglot--hover-info contents range)))
            (hint (thing-at-point 'symbol t)))
        (if blurb
            (with-current-buffer
                (or (and (buffer-live-p sm/eglot-help-buffer)
                         sm/eglot-help-buffer)
                    (setq sm/eglot-help-buffer (generate-new-buffer "*eglot-help*")))
              (with-help-window (current-buffer)
                (rename-buffer (format "*eglot-help for %s*" hint))
                (with-current-buffer standard-output (insert blurb))
                (setq-local nobreak-char-display nil)))
          (display-local-help))))
    'deferred)

  ;; Below is adapted from Doom Emac's handling of integration eglot diagnostics
  ;; with flycheck.

  (setq eglot-stay-out-of '(flymake))

  (defvar-local sm/eglot-flycheck-current-errors nil)

  (defun sm/eglot-flycheck-init (checker callback)
    (eglot-flymake-backend #'sm/eglot-on-diagnostic)
    (funcall callback 'finished sm/eglot-flycheck-current-errors))

  (mapc #'sm/warn-fn-not-bound '(flymake--diag-buffer
                                 flymake--diag-beg
                                 flymake--diag-type
                                 flymake--diag-text
                                 flymake--diag-end))

  (defun sm/eglot-flymake-diag-to-flycheck (diag)
    (with-current-buffer (flymake--diag-buffer diag)
      (flycheck-error-new-at-pos
       (flymake--diag-beg diag)
       (pcase (flymake--diag-type diag)
         ('eglot-note 'info)
         ('eglot-warning 'warning)
         ('eglot-error 'error)
         (_ (error "Unknown diagnostic type %S" diag)))
       (flymake--diag-text diag)
       :end-pos (flymake--diag-end diag)
       :checker 'eglot
       :buffer (current-buffer)
       :filename (buffer-file-name))))

  (defun sm/eglot-on-diagnostic (diags &rest _)
    (setq sm/eglot-flycheck-current-errors
          (mapcar #'sm/eglot-flymake-diag-to-flycheck diags))
    (flycheck-buffer-deferred))

  (flycheck-define-generic-checker 'eglot
    "Report `eglot' diagnostics using `flycheck'."
    :start #'sm/eglot-flycheck-init
    :predicate #'eglot-managed-p
    :modes '(prog-mode text-mode))

  (push 'eglot flycheck-checkers)

  (defun sm/eglot-flycheck-hook ()
    (when (eglot-managed-p)
      (flymake-mode -1)
      (when-let ((current-checker (flycheck-get-checker-for-buffer)))
        (unless (equal current-checker 'eglot)
          (flycheck-add-next-checker 'eglot current-checker)))
      (flycheck-add-mode 'eglot major-mode)
      (flycheck-mode 1)
      (flycheck-buffer-deferred)))

  (add-hook 'eglot-managed-mode-hook #'sm/eglot-flycheck-hook)

  :bind (:map eglot-mode-map
              ("C-c C-d" . sm/eglot-lookup-doc)
              ("C-c C-r" . eglot-rename)))

(provide 'seanmacs-completions)
;;; seanmacs-completions.el ends here
