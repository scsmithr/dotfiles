;;; seanmacs-shell.el --- tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ert-deftest sm/shrink-path-prompt ()
  (should (equal (sm/shrink-path-prompt "/home/sean/dotfiles/emacs/")
                 (cons "~/d/" "emacs")))
  (should (equal (sm/shrink-path-prompt "/ssh:host:/home/sean/dotfiles/emacs/")
                 (cons "ssh:host:~/d/" "emacs")))
  (should (equal (sm/shrink-path-prompt "/ssh:user@host:/home/sean/dotfiles/emacs/")
                 (cons "ssh:user@host:~/d/" "emacs"))))
