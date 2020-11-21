;;; seanmacs-version-control.el --- Version control -*- lexical-binding: t; -*-

;;; Commentary:
;; VC configuration.

;;; Code:

(use-package ediff
  ;; built-in
  :init
  (setq-default ediff-split-window-function #'split-window-horizontally
                ediff-merge-split-window-function #'split-window-horizontally
                ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package magit
  :straight t
  :defer t
  :config
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t)
  (setq magit-module-sections-nested nil
        magit-bury-buffer-function 'magit-mode-quit-window
        magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv)))

(use-package forge
  :straight t
  :after magit
  :config
  (setq forge-pull-notifications nil)
  (transient-append-suffix 'forge-dispatch "c u"
    '("c r" "review pull request" github-review-forge-pr-at-point)))

(use-package github-review
  :straight t
  :defer t
  :commands (github-review-start github-review-forge-pr-at-point))

(use-package evil-magit
  :straight t
  :after magit)

(use-package diff-hl
  :straight t
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)))

(provide 'seanmacs-version-control)
;;; seanmacs-version-control.el ends here

