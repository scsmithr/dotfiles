;;; seanmacs-completions.el --- Completions -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion configuration.

;;; Code:

;; Adapted from https://github.com/minad/consult/blob/1a6ed29e92f00266daff4ff5f62602f53ef7d158/consult.el#L2284
(defun sm/default-completion-in-region (start end collection predicate)
  "A very simple completion-in-read function using `completing-read'.

Fallback for when emacs is launched from the terminal (since
child frames aren't available)."
  (let* ((prompt "Completion: ")
         (initial (buffer-substring-no-properties start end))
         (buffer (current-buffer))
         (completion (completing-read prompt
                                      (if (functionp collection)
                                          (lambda (&rest args)
                                            (with-current-buffer buffer
                                              (apply collection args)))
                                        collection)
                                      predicate nil initial)))
    (if completion
        (progn
          (completion--replace start end (setq completion (concat completion)))
          (when-let (exit-fn (plist-get completion-extra-properties :exit-function))
            (funcall exit-fn completion
                     (if (eq (try-completion completion collection predicate) t)
                         'finished
                       'exact)))
          t)
      (progn
        (message "No completion")
        nil))))

(use-package corfu
  :straight t
  :init
  (setq completion-in-region-function #'sm/default-completion-in-region)

  (setq tab-always-indent 'complete)
  (setq corfu-preview-current t
        corfu-auto nil)
  (corfu-global-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        orderless-matching-styles '(orderless-literal orderless-regexp)))

(use-package vertico
  :straight t
  :init
  (setq vertico-count 10
        vertico-resize nil)
  (vertico-mode))

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

(use-package imenu-list
  :straight t
  :init
  (setq imenu-list-position 'right
        imenu-list-size 44)

  (defun sm/imenu-list-install-display-buffer ()
    "Use a side window for the imenu-list buffer."
    (let* ((side (cond ((eq imenu-list-position 'above) 'top)
                       ((eq imenu-list-position 'below) 'bottom)
                       (t imenu-list-position)))
           (size-sym (if (or (eq side 'top)
                             (eq side 'bottom))
                         'window-height
                       'window-width)))
      (setf (alist-get (concat "^" (regexp-quote imenu-list-buffer-name) "$")
                       display-buffer-alist nil nil #'equal)
            `(display-buffer-in-side-window
              (,size-sym . ,imenu-list-size)
              (slot . 0)
              (side . ,side)
              (dedicated . t)))))

  (advice-add 'imenu-list-install-display-buffer :override
              #'sm/imenu-list-install-display-buffer)

  (defun sm/imenu-list-show ()
    (interactive)
    (sm/save-window-excursion
     (call-interactively 'imenu-list-goto-entry)))

  (evil-collection-define-key 'normal 'imenu-list-major-mode-map
    (kbd "SPC") #'sm/imenu-list-show)

  :bind (("C-c b l" . imenu-list-smart-toggle)))

(use-package xref
  ;; built-in, ish. Eglot pulls in development versions.
  :straight t
  :config
  (defun sm/xref-show ()
    "Show xref result under point, keeping cursor in the xref window."
    (interactive)
    (sm/save-window-excursion
     (xref-goto-xref)))

  (evil-collection-define-key 'normal 'xref--xref-buffer-mode-map
    "p" #'sm/pop-to-some-window
    (kbd "SPC") #'sm/xref-show))

(use-package eldoc
  ;; built-in, ish. Eglot pulls in development versions.
  :straight t
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

  ;; Below is adapted from Doom Emacs' handling of integration eglot diagnostics
  ;; with flycheck.

  (setq eglot-stay-out-of '(flymake))

  (defvar-local sm/eglot-flycheck-current-errors nil)

  (defun sm/eglot-flycheck-init (checker callback)
    (eglot-flymake-backend #'sm/eglot-on-diagnostic)
    (funcall callback 'finished sm/eglot-flycheck-current-errors))

  (mapc #'sm/warn-fn-not-bound '(flymake--diag-beg
                                 flymake--diag-type
                                 flymake--diag-text
                                 flymake--diag-end))

  (defun sm/eglot-flymake-diag-to-flycheck (diag)
    (with-current-buffer (flymake-diagnostic-buffer diag)
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

  ;; See: https://github.com/joaotavora/eglot/pull/771#issuecomment-1030710724
  (defun sm/eglot-remove-company-docsig (completions)
    "Remove the :company-docsig property from the properties list in COMPLETIONS.

Selectrum will attempt to funcall the function associated with
:company-docsig, and eglot currently attempts to use the lsp
server associated with the buffer. Unfortunately this breaks when
using selectrum for completions since there won't be a server
associated with the minibuffer. To avoid errors, just remove it."
    (when completions
      (let ((plist (nthcdr 3 completions)))
        (map-delete plist :company-docsig)
        `(,(nth 0 completions)
          ,(nth 1 completions)
          ,(nth 2 completions)
          ,@plist))))

  (advice-add 'eglot-completion-at-point :filter-return #'sm/eglot-remove-company-docsig)

  :bind (:map eglot-mode-map
              ("C-c C-d" . sm/eglot-lookup-doc)
              ("C-c C-r" . eglot-rename)))

(provide 'seanmacs-completions)
;;; seanmacs-completions.el ends here
