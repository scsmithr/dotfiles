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

(provide 'keybinds)
;;; keybinds.el ends here
