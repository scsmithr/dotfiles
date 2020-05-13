;;; seanmacs-keybinds.el --- Core keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Keybind configuration.

;;; Code:

(defvar core-leader-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defvar core-leader-key "SPC"
  "Leader key.")

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

;; Applications
(core/leader
 "a e" 'mu4e
 "a d" 'docker
 "a g s" 'gcloud-instance-shell
 "a g e" 'gcloud-instance-eshell
 "a k" 'kube)

;; Dired/file management
(core/leader
 "d d" 'dired
 "d p" 'seanmacs/window-dired-project-left
 "d f" 'find-file)

;; Org
(core/leader
 "x x" 'org-capture
 "x l" 'org-agenda-list
 "x s" 'org-search-view)

;; Magit
(core/leader
 "g g" 'magit-status
 "g f" 'magit-file-dispatch)

;; Shell
(core/leader
 "s s" 'eshell
 "s n" 'eshell-new
 "s p" 'projectile-run-eshell)

;; Window management
(core/leader
 "w w" 'seanmacs/split-window-right
 "w h" 'seanmacs/split-window-below
 "w H" 'shrink-window-horizontally
 "w L" 'enlarge-window-horizontally
 "w K" 'shrink-window
 "w J" 'enlarge-window
 "w =" 'balance-windows
 "w u" 'winner-undo
 "w p" 'seanmacs/use-some-window
 "." 'repeat)

;; Buffer management
(core/leader
 "b r" 'seanmacs/rename-buffer-special
 "b v" 'revert-buffer
 "b k" 'kill-this-buffer
 "b i" 'seanmacs/indent-buffer)

;; Other
(core/leader
 "o b" 'ibuffer
 "o s" 'imenu)

;; Projectile
(core/leader
   "p" 'projectile-command-map)

;; Flycheck
(core/leader
   "f" 'flycheck-command-map)

;; Xref
(core/leader
   "r a" 'xref-find-apropos
   "r r" 'xref-find-references)

(defun seanmacs/evil-window-next-skip-dired-sidebar ()
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
  (setq evil-collection-key-blacklist
        (list core-leader-key))
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config (global-evil-surround-mode 1))

(provide 'seanmacs-keybinds)
;;; seanmacs-keybinds.el ends here
