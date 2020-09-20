;;; seanmacs-org.el --- Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Org configuration.

;;; Code:

(defvar seanmacs/notes-dir "~/syncthing/notes/"
  "Directory containging all of my notes (including org files).")

(use-package org
  :straight t
  :config
  (setq org-agenda-files (list seanmacs/notes-dir)
        org-agenda-restore-windows-after-quit t
        org-agenda-span 'week
        org-agenda-window-setup 'current-window)

  ;; Enable basic movement keys in agenda.
  (evil-add-hjkl-bindings org-agenda-mode-map 'emacs)

  (setq org-default-notes-file (concat seanmacs/notes-dir "refile.org")
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path t
        org-template-directory "~/.emacs.d/org-templates"
        org-startup-folded nil
        org-startup-with-inline-images t
        org-hide-leading-stars t
        ;; I prefer having blank lines between subtrees when unfolded, but
        ;; no blank lines when folded.
        org-blank-before-new-entry (quote ((heading . always) (plain-list-item . always)))
        org-enforce-todo-dependencies t
        org-imenu-depth 9
        org-outline-path-complete-in-steps nil ;; Show entire path when refiling.
        org-confirm-babel-evaluate nil ;; I trust myself.
        org-fontify-done-headline nil
        org-hide-emphasis-markers nil
        org-src-tab-acts-natively t
        ;; Keep layout, just use different window than current. Useful to see
        ;; both the org buffer and the source at the same time.
        org-src-window-setup 'other-window)

  ;; Embedded latex images are a bit small by default.
  (plist-put org-format-latex-options :scale 1.7)

  (setq org-capture-templates
        `(
          ("n" "Note" entry (file+headline "" "Notes")
           (file ,(concat org-template-directory "/note"))
           :empty-lines 1)
          ("t" "Task" entry (file+headline "" "Tasks")
           (file ,(concat org-template-directory "/task"))
           :empty-lines 1)))

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELED")))
  :bind (:map org-mode-map
              ("C-c t" . org-toggle-narrow-to-subtree)))

(use-package ob
  :after (org ob-http gnuplot ob-async)
  :config

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
     (sql        . t)
     (python     . t)))
  :hook ((org-babel-after-execute . org-display-inline-images)))

(use-package ob-http
  :straight t)

(use-package gnuplot
  :straight t)

(use-package ob-async
  :straight t)

(provide 'seanmacs-org)
;;; seanmacs-org.el ends here
