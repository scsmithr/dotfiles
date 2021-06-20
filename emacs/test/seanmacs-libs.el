;;; seanmacs-libs.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(ert-deftest sm/parse-filepath ()
  (should (equal (sm/parse-filepath "/home/sean/dotfiles")
                 (make-sm/filepath :path "/home/sean/dotfiles")))
  (should (equal (sm/parse-filepath "/ssh:user@host:~/some/path")
                 (make-sm/filepath :path "~/some/path"
                                   :tramp-method "ssh"
                                   :tramp-host "user@host"))))

