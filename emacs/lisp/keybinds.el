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

(defun evilify-window-switch (map)
  (define-key map (kbd "C-w C-w") 'evil-window-next-skip-treemacs)
  (define-key map (kbd "C-w w") 'evil-window-next-skip-treemacs))

(defun evilify-maps (maps)
  "Naively add evil like keybinds to MAPS.  Does not handle shadowed keybinds."
  (let ((map (car maps))
        (rest (cdr maps)))

    (define-key map "/" 'evil-search-forward)
    (define-key map ":" 'evil-ex)
    (define-key map "h" 'evil-backward-char)
    (define-key map "j" 'evil-next-visual-line)
    (define-key map "k" 'evil-previous-visual-line)
    (define-key map "l" 'evil-forward-char)
    (define-key map "n" 'evil-search-next)
    (define-key map "N" 'evil-search-previous)
    (define-key map "v" 'evil-visual-char)
    (define-key map "V" 'evil-visual-line)
    (define-key map "gg" 'evil-goto-first-line)
    (define-key map "G" 'evil-goto-line)
    (define-key map (kbd "C-f") 'evil-scroll-page-down)
    (define-key map (kbd "C-b") 'evil-scroll-page-up)
    (define-key map (kbd "C-e") 'evil-scroll-line-down)
    (define-key map (kbd "C-y") 'evil-scroll-line-up)
    (define-key map (kbd "C-d") 'evil-scroll-down)
    (define-key map (kbd "C-u") 'evil-scroll-up)
    (define-key map (kbd "C-z") 'evil-emacs-state)

    (evilify-window-switch map)

    (when rest (evilify-maps rest))))

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
   "wh" 'evil-window-split
   "Wh" 'shrink-window-horizontally
   "Wl" 'enlarge-window-horizontally
   "Wk" 'shrink-window
   "Wj" 'enlarge-window
   "w=" 'balance-windows
   "." 'repeat)
  ;; Overwrite the default next window commands with one that skips treemacs.
  (define-key evil-window-map "C-w" #'evil-window-next-skip-treemacs)
  (define-key evil-window-map "w" #'evil-window-next-skip-treemacs)
  (define-key evil-motion-state-map (kbd "C-w w") #'evil-window-next-skip-treemacs)
  (define-key evil-motion-state-map (kbd "C-w C-w") #'evil-window-next-skip-treemacs))

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

(defun async-shell-buffer (program &rest args)
  (interactive)
  (let* ((command (string-join (append (list program) args) " "))
         (output-buffer (concat "*" command "*")))
    (async-shell-command command output-buffer)))

(evil-define-command evil:shell (args)
  (interactive "<sh>")
  (async-shell-buffer args))

(evil-define-command evil:project-shell (args)
  (interactive "<sh>")
  (let ((command (concat "cd " (projectile-project-root) "&&" args))
        (output-buffer (concat "*" args "*")))
    (async-shell-command args output-buffer)))

(evil-ex-define-cmd "sh[ell]" #'evil:shell)
(evil-ex-define-cmd "psh[ell]" #'evil:project-shell)

(evil-ex-define-cmd "psql" #'sql-postgres)

(provide 'keybinds)
;;; keybinds.el ends here
