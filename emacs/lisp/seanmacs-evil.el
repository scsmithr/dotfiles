;;; seanmacs-evil.el --- Evil keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil configuration.

;;; Code:

(use-package winner
  ;; built-in
  :config
  (winner-mode 1))

;; evil
(use-package evil
  :straight (:host github :repo "scsmithr/evil" :branch "fix-temporary-goal-column")
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-shift-round nil
        evil-want-C-u-scroll t
        evil-want-fine-undo t
        evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode)
  ;; Removing these from the insert map allows using them to select items in
  ;; corfu.
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
  :init
  (setq forge-add-default-bindings nil)
  :config
  (defvar sm/evil-collection-disabled-modes
    '(go-mode corfu)
    "Modes that should not have modified keybinds.")

  (dolist (mode sm/evil-collection-disabled-modes)
    (setq evil-collection-mode-list (delete mode evil-collection-mode-list)))

  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config (global-evil-surround-mode 1))

(provide 'seanmacs-evil)
;;; seanmacs-keybinds.el ends here
