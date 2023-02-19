;;; seanmacs-completions.el --- Completions -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion configuration.

;;; Code:

(eval-when-compile
  (require 'use-package))

;; Adapted from https://github.com/minad/consult/blob/1a6ed29e92f00266daff4ff5f62602f53ef7d158/consult.el#L2284
(defun sm/default-completion-in-region (start end collection predicate)
  "A very simple completion-in-read function using `completing-read'.

Fallback for when child frame based completions aren't
available (for example while using Emacs in the terminal or
completing inside the minibuffer)."
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
  (setq enable-recursive-minibuffers t)
  (setq completion-in-region-function #'sm/default-completion-in-region)

  (setq tab-always-indent 'complete)
  (setq corfu-preview-current t
        corfu-auto nil)
  (global-corfu-mode)
  :bind (:map corfu-map
              ("C-j" . corfu-quit)
              ("SPC" . corfu-insert-separator)))

(use-package cape
  :straight t)

(use-package orderless
  :straight t
  :init
  (defvar sm/default-orderless-matching-styles
    '(orderless-literal orderless-regexp)
    "Default matching styles for orderless.")

  (defun sm/first-includes-flex (pattern index total)
    "First component will also be matched with flex."
    (when (= index 0) (append sm/default-orderless-matching-styles
                              '(orderless-flex))))

  (setq orderless-matching-styles sm/default-orderless-matching-styles
        orderless-style-dispatchers '(sm/first-includes-flex))

  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles basic partial-completion)))
        completion-ignore-case t))

(use-package vertico
  :straight t
  :init
  (setq vertico-count 10
        vertico-resize nil)
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-exit-input)))

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

(use-package xref
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
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

(use-package eglot
  :commands (eglot eglot-ensure sm/add-server-program)
  :config
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))

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

  :bind (:map eglot-mode-map
              ("C-c C-d" . sm/eglot-lookup-doc)
              ("C-c C-r" . eglot-rename)))

(provide 'seanmacs-completions)
;;; seanmacs-completions.el ends here
