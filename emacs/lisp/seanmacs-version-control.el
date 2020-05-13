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
  (setq magit-completing-read-function 'magit-ido-completing-read)
  :init
  (core/leader
   "gg" 'magit-status
   "gf" 'magit-file-dispatch))

(use-package forge
  :straight t
  :after magit
  :config
  (setq forge-pull-notifications nil))

(use-package evil-magit
  :straight t
  :after magit)

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)))

(provide 'seanmacs-version-control)
;;; seanmacs-version-control.el ends here

