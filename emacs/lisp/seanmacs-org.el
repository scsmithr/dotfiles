;;; seanmacs-org.el --- Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Org configuration.

;;; Code:

(defvar seanmacs/notes-dir "~/syncthing/notes/"
  "Directory containging all of my notes (including org files).")

(use-package org
  :straight t
  :config
  ;; Workaround for fontlocking extending beyond the text of priority cookies.
  ;; See: https://gitlab.com/protesilaos/modus-themes/-/issues/95
  (setq org-priority-regexp ".*?\\(\\[#\\([A-Z0-9]+\\)\\]\\) ?")

  (setq org-agenda-files (list seanmacs/notes-dir)
        org-agenda-restore-windows-after-quit t
        org-agenda-span 3
        org-agenda-window-setup 'current-window
        ;; Default with 'require-timed' removed. I always want to see the time
        ;; grid for today.
        org-agenda-time-grid
        '((daily today)
          (800 1000 1200 1400 1600 1800 2000)
          "......"
          "----------------")
        ;; Mostly default prefixes. Removes icon strings, and adds breadcrumbs
        ;; (%b) to the search view.
        org-agenda-prefix-format
        '((agenda . " %-12:c%?-12t% s")
          (todo . " %-12:c")
          (tags . " %-12:c")
          (search . " %-12:c %b"))
        org-agenda-breadcrumbs-separator " > "
        org-agenda-search-view-always-boolean t)

  ;; Enable basic movement keys in agenda.
  (evil-add-hjkl-bindings org-agenda-mode-map 'emacs)

  (setq org-default-notes-file (concat seanmacs/notes-dir "refile.org")
        org-archive-location "archive/%s_archive::" ;; Keep top level directory clean.
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

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELED")))

  (setq org-priority-highest ?A
        org-priority-lowest ?C
        org-priority-default ?C)

  (setq org-capture-templates
        `(
          ("n" "Note" entry (file+headline "" "Notes")
           (file ,(concat org-template-directory "/note"))
           :empty-lines 1)
          ("t" "Task" entry (file+headline "" "Tasks")
           (file ,(concat org-template-directory "/task"))
           :empty-lines 1)))

  ;; Keep track when I reschedule or complete things.
  (setq org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time)

  ;; Embedded latex images are a bit small by default.
  (plist-put org-format-latex-options :scale 1.7)

  :hook ((org-capture-mode . evil-insert-state))
  :bind (("C-c o o" . org-capture)
         ("C-c o a" . org-agenda)
         ("C-c o l" . org-agenda-list)
         ("C-c o t" . org-todo-list)
         ("C-c o s" . org-search-view)
         :map org-mode-map
         ("C-c t" . org-toggle-narrow-to-subtree)))

(use-package ob
  :after (org ob-http ob-go gnuplot ob-async)
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
     (go         . t)
     (python     . t)))
  :hook ((org-babel-after-execute . org-display-inline-images)))

(use-package ob-http
  :straight t)

(use-package gnuplot
  :straight t)

(use-package ob-go
  :straight t)

(use-package ob-async
  :straight t)

(provide 'seanmacs-org)
;;; seanmacs-org.el ends here
