;;; seanmacs-straight.el --- Utilities for straight -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'straight)

(defun sm/straight-lock-file-rel (dir)
  "Get the straight lockfile relative to DIR."
  (concat (file-name-as-directory dir)
          "straight/versions/default.el"))

(defvar sm/straight-lock-file-user-dir
  (sm/straight-lock-file-rel user-emacs-directory))

(defvar sm/straight-lock-file-dotfiles
  (sm/straight-lock-file-rel (concat sm/dotfiles-dir "emacs/")))

(defun sm/straight-update-rm-lock ()
  "Update all packages and remove the existing lockfile."
  (interactive)
  (straight-pull-recipe-repositories)
  (straight-pull-all)
  (delete-file sm/straight-lock-file-user-dir))

(defun sm/straight-freeze-and-copy-lock ()
  "Copy the frozen lockfile to my dotfiles repo."
  (interactive)
  (delete-file sm/straight-lock-file-user-dir)
  (straight-freeze-versions)
  (copy-file sm/straight-lock-file-user-dir sm/straight-lock-file-dotfiles t))

(provide 'seanmacs-straight)
;;; seanmacs-straight.el ends here

