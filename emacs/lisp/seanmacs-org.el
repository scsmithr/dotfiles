;;; seanmacs-org.el --- Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Org configuration.

;;; Code:

(core/leader
 "x x" 'org-capture
 "x a" 'org-agenda)

(use-package org
  :straight (org :type built-in)
  :config
  (setq org-default-notes-file "~/notes/refile.org"
        org-agenda-files (list "~/notes/")
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-template-directory "~/.emacs.d/org-templates"
        org-startup-folded nil
        org-hide-leading-stars t
        org-blank-before-new-entry (quote ((heading . always) (plain-list-item . always)))
        org-enforce-todo-dependencies t)

  (setq org-capture-templates
        `(
          ("n" "Note" entry (file+headline "" "Notes")
           (file ,(concat org-template-directory "/note")))
          ("t" "Task" entry (file+headline "" "Tasks")
           (file ,(concat org-template-directory "/task")))))

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELED")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((shell . t)))))

(use-package ob-restclient
  :straight t)

(use-package ob-http
  :straight t)

(provide 'seanmacs-org)
;;; seanmacs-org.el ends here

