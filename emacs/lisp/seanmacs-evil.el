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
  :straight t
  :init
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-shift-round nil
        evil-want-C-u-scroll t
        ;; C-i and TAB being the same thing messes with some keybinds (e.g. org
        ;; headline toggle)
        evil-want-C-i-jump nil
        evil-want-fine-undo t
        evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo)

  :init
  ;; See https://github.com/emacs-evil/evil/issues/1297
  ;;
  ;; Temporary goal column occasionally is set to an negative value with
  ;; display-line-numbers-mode and whitespace-mode both enabled. Evil uses this
  ;; value when calculating which column to use after some motions. This fix
  ;; just ensures that the column is never negative.
  (defun sm/fix-negative-column (args)
    (list (max (car args) 0)))

  (advice-add 'line-move-to-column :filter-args #'sm/fix-negative-column)

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
