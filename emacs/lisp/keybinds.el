;;; keybinds.el --- Core keybindings -*- lexical-binding: t; -*-

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

(defun evil-window-next-skip-treemacs ()
  "Move the cursor to next window in cyclic order, skipping
Treemacs buffers."
  (interactive)
  (select-window (next-window))
  (when (string-match-p (regexp-quote "Treemacs") (buffer-name))
    (select-window (next-window))))

;; evil
(use-package evil
  :straight t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  (core/leader
   "ww" 'evil-window-vsplit
   "wh" 'evil-window-split)
  ;; Overwrite the default next window commands with one that skips treemacs.
  (define-key evil-window-map "C-w" #'evil-window-next-skip-treemacs)
  (define-key evil-window-map "w" #'evil-window-next-skip-treemacs)
  (define-key evil-motion-state-map (kbd "C-w w") #'evil-window-next-skip-treemacs)
  (define-key evil-motion-state-map (kbd "C-w C-w") #'evil-window-next-skip-treemacs)
  ;; rebind ctrl-p
  (define-key evil-normal-state-map (kbd "C-p") #'projectile-find-file))

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
        (list core-leader-key core-local-leader-key))
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :config (global-evil-surround-mode 1))

(evil-ex-define-cmd "sh" #'async-shell-command)

(provide 'keybinds)
;;; keybinds.el ends here
