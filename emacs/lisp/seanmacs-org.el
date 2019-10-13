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

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it)))

(use-package ob-http
  :straight t)

(use-package gnuplot
  :straight t)

(provide 'seanmacs-org)
;;; seanmacs-org.el ends here
