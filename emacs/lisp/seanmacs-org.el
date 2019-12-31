;;; seanmacs-org.el --- Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Org configuration.

;;; Code:

(core/leader
 "x x" 'org-capture
 "x a" 'org-agenda)

(use-package org
  :defer t
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
        '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELED"))))

(use-package ob
  ;; built-in
  :after org
  :config
  (require 'ob-http)
  (require 'gnuplot)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (js         . t)
     (emacs-lisp . t)
     (lisp       . t)
     (haskell    . t)
     (sql        . t)
     (http       . t)
     (gnuplot    . t)
     (calc       . t)
     (python     . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))

(use-package ob-http
  :straight t
  :defer t)

(use-package gnuplot
  :straight t
  :defer t)

(provide 'seanmacs-org)
;;; seanmacs-org.el ends here
