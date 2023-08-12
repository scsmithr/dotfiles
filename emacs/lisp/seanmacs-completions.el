;;; seanmacs-completions.el --- Completions -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion configuration.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package corfu
  :straight t
  :init
  (setq enable-recursive-minibuffers t)

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  (setq tab-always-indent 'complete)

  (setq corfu-preview-current t
        corfu-auto nil
        corfu-max-width 80)

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
  :straight (:host github :repo "minad/vertico"
                   :files (:defaults "extensions/*"))
  :init
  (setq vertico-count 10
        vertico-resize nil
        vertico-sort-function #'vertico-sort-history-alpha)

  (vertico-mode)
  :bind (:map vertico-map
              ("M-N" . vertico-next-group)
              ("M-P" . vertico-previous-group)
              ("C-j" . vertico-exit-input)))

(use-package marginalia
  :straight t
  :init
  (setq marginalia-align 'left
        marginalia-align-offset 25)
  (marginalia-mode))

(use-package consult
  :straight t
  :bind (
         ("C-c h" . consult-history)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         :map buffer-prefix-map
         ("s" . consult-imenu)
         ("l" . consult-line)
         ("f" . consult-focus-lines)
         :map project-prefix-map
         ("r" . consult-ripgrep)
         ("b" . consult-project-buffer)
         ))

(use-package imenu
  ;; built-in
  :config
  (setq imenu-auto-rescan t
        imenu-space-replacement " "))

(use-package xref
  ;; built-in
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

(use-package eglot
  :commands (eglot eglot-ensure sm/add-server-program)
  :config
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider
                                            :inlayHintProvider))

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
