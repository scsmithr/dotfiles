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

(defun core/set-leader-major-mode-keys (mode key def &rest bindings)
  "Set mode local keybinds on the local leader map."
  (let* ((hook (intern (format "%s-hook" mode))))
    (add-hook hook
              (lambda ()
                (let ((mode-map (make-sparse-keymap)))
                  (core/set-leader-keys core-local-leader-key mode-map)
                  (while key
                    (define-key mode-map (kbd key) def)
                    (setq key (pop bindings) def (pop bindings))))))))

(defalias 'core/local 'core/set-leader-major-mode-keys)

(provide 'keybinds)
;;; keybinds.el ends here
