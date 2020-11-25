;;; seanmacs-keybinds.el --- Core keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Keybind configuration.

;;; Code:

(defun seanmacs/evil-window-next-skip-dired-sidebar ()
  "Move the cursor to next window in cyclic order, skipping dired sidebar buffers."
  (interactive)
  (select-window (next-window))
  (when (string-prefix-p "*Dired Project:" (buffer-name))
    (select-window (next-window))))

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
  ;; Overwrite the default next window commands with one that skips dired
  ;; sidebar.
  (define-key evil-window-map "C-w" #'seanmacs/evil-window-next-skip-dired-sidebar)
  (define-key evil-window-map "w" #'seanmacs/evil-window-next-skip-dired-sidebar)
  (define-key evil-motion-state-map (kbd "C-w w") #'seanmacs/evil-window-next-skip-dired-sidebar)
  (define-key evil-motion-state-map (kbd "C-w C-w") #'seanmacs/evil-window-next-skip-dired-sidebar))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config (global-evil-surround-mode 1))

(provide 'seanmacs-keybinds)
;;; seanmacs-keybinds.el ends here
