;;; seanmacs-keybinds.el --- Core keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Keybind configuration.

;;; Code:

(defvar core-leader-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defvar core-leader-key "SPC"
  "Leader key.")

(defvar core-local-leader-key "m"
  "Local leader key for major mode keybinds.")

(defun core/init-leader ()
  "Initialize the leader map."
  (progn
    (define-key evil-normal-state-map (kbd core-leader-key) core-leader-map)
    (define-key evil-visual-state-map (kbd core-leader-key) core-leader-map)))

(defun core/set-leader-keys (key def &rest bindings)
  "Set global keybinds on the leader map."
  (while key
    (define-key core-leader-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(defalias 'core/leader 'core/set-leader-keys)

(defun core/set-local-leader-keys (map key def &rest bindings)
  "Set mode specific keybinds."
  (while key
    (evil-collection-define-key 'normal map
      (kbd (format "%s %s%s" core-leader-key core-local-leader-key key)) def)
    (setq key (pop bindings) def (pop bindings))))

(defalias 'core/local 'core/set-local-leader-keys)

(defun evil-window-next-skip-dired-sidebar ()
  "Move the cursor to next window in cyclic order, skipping dired sidebar buffers."
  (interactive)
  (select-window (next-window))
  (when (string-prefix-p "*Dired Side:" (buffer-name))
    (select-window (next-window))))

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
        evil-want-keybinding nil)
  :config
  (evil-mode)
  (evil-update-insert-state-bindings "\C-n" t)
  (evil-update-insert-state-bindings "\C-p" t)
  ;; Overwrite the default next window commands with one that skips dired
  ;; sidebar.
  (define-key evil-window-map "C-w" #'evil-window-next-skip-dired-sidebar)
  (define-key evil-window-map "w" #'evil-window-next-skip-dired-sidebar)
  (define-key evil-motion-state-map (kbd "C-w w") #'evil-window-next-skip-dired-sidebar)
  (define-key evil-motion-state-map (kbd "C-w C-w") #'evil-window-next-skip-dired-sidebar))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (setq evil-collection-key-blacklist
        (list core-leader-key))
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :config (global-evil-surround-mode 1))

(provide 'seanmacs-keybinds)
;;; seanmacs-keybinds.el ends here
