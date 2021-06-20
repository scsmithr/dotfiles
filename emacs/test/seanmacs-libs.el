;;; seanmacs-libs.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(ert-deftest sm/path-localname ()
  (should (equal (sm/path-localname "/home/sean/dotfiles") "/home/sean/dotfiles"))
  (should (equal (sm/path-localname "/ssh:user@host:/home/sean/dotfiles") "/home/sean/dotfiles")))
