;;; seanmacs-keybinds.el --- Core keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Keybind configuration.

;;; Code:

(use-package undo-fu
  :straight t)

;; evil
(use-package evil
  :straight t
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu)

  ;; C-i is the same as tab in the terminal.
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil))

  :config
  (evil-mode)
  (evil-update-insert-state-bindings "\C-n" t)
  (evil-update-insert-state-bindings "\C-p" t)

  (defun sm/evil-define-window-key (key def)
    (define-key evil-window-map key def)
    (define-key evil-motion-state-map (kbd (concat "C-w " key)) def))

  (sm/evil-define-window-key "u" #'winner-undo))


(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-company-use-tng nil) ;; Compat with company 0.9.13
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config (global-evil-surround-mode 1))

(provide 'seanmacs-keybinds)
;;; seanmacs-keybinds.el ends here
