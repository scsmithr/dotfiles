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
                ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; HACK: Evil collection bindings don't seem to get set immediately. Not sure
  ;; which upgrade broke this.
  :hook ((ediff-startup . evil-force-normal-state)))

(use-package magit
  :straight t
  :config
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t)
  (setq magit-blame-echo-style 'margin
        magit-module-sections-nested nil
        magit-diff-refine-hunk t ;; Show whitespace changes in status buffer.
        magit-bury-buffer-function 'magit-mode-quit-window
        magit-section-visibility-indicator '(right-arrow . down-arrow))
  :bind (:map git-prefix-map
              ("g" . magit-status)
              ("f" . magit-file-dispatch)))

(use-package orgit
  :straight t)

(use-package forge
  :straight t
  :config
  (setq forge-pull-notifications nil))

(use-package git-link
  :straight t
  :config
  (setq git-link-use-commit t
        git-link-open-in-browser t)
  :bind (:map git-prefix-map
              ("o" . git-link)))

(provide 'seanmacs-version-control)
;;; seanmacs-version-control.el ends here

