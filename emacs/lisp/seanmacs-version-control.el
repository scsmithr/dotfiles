;;; seanmacs-version-control.el --- Version control -*- lexical-binding: t; -*-

;;; Commentary:
;; VC configuration.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ediff
  ;; built-in
  :init
  (setq-default ediff-split-window-function #'split-window-horizontally
                ediff-merge-split-window-function #'split-window-horizontally
                ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package magit
  :straight t
  :config
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t)
  (setq magit-blame-echo-style 'margin
        magit-module-sections-nested nil
        magit-diff-refine-hunk t ;; Show whitespace changes in status buffer.
        magit-bury-buffer-function 'magit-mode-quit-window
        magit-section-visibility-indicator '(right-arrow . down-arrow))
  :bind (("C-c g g" . magit-status)
         ("C-c g f" . magit-file-dispatch)))

(use-package orgit
  :straight t)

(use-package forge
  :straight t
  :config
  (setq forge-pull-notifications nil))

(use-package transient
  :straight t)

(use-package diff-hl
  :straight t
  :demand t ;; Doesn't like to come on.
  :config
  (setq diff-hl-draw-borders nil)
  (defun sm/diff-hl-fringe-bmp (_type _pos)
    'sm/left-line-bmp)
  (setq diff-hl-fringe-bmp-function #'sm/diff-hl-fringe-bmp)
  (global-diff-hl-mode)
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)))

(use-package git-link
  :straight t
  :config
  (setq git-link-use-commit t
        git-link-open-in-browser t)

  :bind (("C-c g o" . git-link)))

(provide 'seanmacs-version-control)
;;; seanmacs-version-control.el ends here

