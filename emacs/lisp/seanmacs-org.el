;;; seanmacs-org.el --- Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Org configuration.

;;; Code:

(defvar sm/notes-dir "~/syncthing/notes/"
  "Directory containging all of my notes (including org files).")

(defvar sm/org-capture-templates-dir "~/.emacs.d/org-templates"
  "Directory containing capture templates.")

(use-package org
  :straight t
  :config
  ;; Enable basic movement keys in agenda.
  ;; Previous binds:
  ;; h - org-agenda-holidays
  ;; j - org-agenda-goto-date
  ;; k - org-agenda-capture
  ;; l - org-agenda-log-mode
  (evil-add-hjkl-bindings org-agenda-mode-map 'emacs)

  (setq org-default-notes-file (concat sm/notes-dir "refile.org")
        org-archive-location "archive/%s_archive::" ;; Keep top level directory clean.
        org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path t
        org-startup-folded nil
        org-startup-with-inline-images t
        org-hide-leading-stars t
        ;; I prefer having blank lines between subtrees when unfolded, but
        ;; no blank lines when folded.
        org-blank-before-new-entry (quote ((heading . always) (plain-list-item . always)))
        org-cycle-separator-lines 0 ;; Never show blank lines.
        org-enforce-todo-dependencies t
        org-imenu-depth 9
        org-outline-path-complete-in-steps nil ;; Show entire path when refiling.
        org-confirm-babel-evaluate nil ;; I trust myself.
        org-catch-invisible-edits 'show-and-error ;; I don't trust myself.
        org-fontify-done-headline nil
        org-adapt-indentation t
        org-hide-emphasis-markers nil
        org-src-tab-acts-natively t
        ;; Keep layout, just use different window than current. Useful to see
        ;; both the org buffer and the source at the same time.
        org-src-window-setup 'other-window
        org-log-into-drawer t
        org-global-properties '(("Effort_ALL" . "5min 15min 30min 1h 2h 4h 8h 1d")))

  (setq org-use-fast-todo-selection 'expert
        org-fast-tag-selection-single-key 'expert)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b)" "IN-PROGRESS(i)" "|" "DELEGATED(e)" "DONE(d)" "CANCELED(c)")))

  (setq org-agenda-custom-commands
        '(("d" "Day overview"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-deadline-warning-days 1)))
            (todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "In progress")
                   (org-agenda-prefix-format "%i %-12:c [%-5e] ")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Up next")
                   (org-agenda-prefix-format "%i %-12:c [%-5e] ")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "Completed Today")))))
          ("u" "Unscheduled"
           ((todo ""
                  ((org-agenda-overriding-header "Unscheduled")
                   (org-agenda-prefix-format "%i %-12:c [%-5e] ")
                   (org-agenda-todo-ignore-scheduled 'all)))))
          ("b" "Blocked"
           ((todo "BLOCKED"
                  ((org-agenda-overriding-header "Blocked")))))
          ("r" "Refile overview"
           ((tags-todo "refile+task"
                       ((org-agenda-overriding-header "Tasks")
                        (org-agenda-prefix-format "%i %-12:c [%-5e] ")))
            (tags "refile+note+LEVEL=2"
                  ((org-agenda-overriding-header "Notes")))))))

  (setq org-agenda-files (list sm/notes-dir)
        org-agenda-restore-windows-after-quit t
        org-agenda-span 'fortnight
        org-agenda-window-setup 'current-window
        ;; Default with 'require-timed' removed. I always want to see the time
        ;; grid for today.
        org-agenda-time-grid
        '((daily today)
          (800 1000 1200 1400 1600 1800 2000)
          "......"
          "----------------")
        org-agenda-current-time-string "** now **"
        ;; Mostly default prefixes. Removes icon strings, and adds breadcrumbs
        ;; (%b) to the search view.
        org-agenda-prefix-format
        '((agenda . " %-12:c%?-12t% s")
          (todo . " %-12:c")
          (tags . " %-12:c")
          (search . " %-12:c %b"))
        org-agenda-breadcrumbs-separator " > "
        org-agenda-search-view-always-boolean t
        org-show-context-detail '((default . canonical)))

  (setq org-priority-highest ?A
        org-priority-lowest ?C
        org-priority-default ?C)

  (setq org-capture-templates
        `(
          ("a" "Annotate" entry (file+headline "" "Notes")
           (file ,(concat sm/org-capture-templates-dir "/annotate"))
           :empty-lines 1)
          ("n" "Note" entry (file+headline "" "Notes")
           (file ,(concat sm/org-capture-templates-dir "/note"))
           :empty-lines 1)
          ("t" "Task" entry (file+headline "" "Tasks")
           (file ,(concat sm/org-capture-templates-dir "/task"))
           :empty-lines 1)))

  ;; Keep track when I reschedule or complete things.
  (setq org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time)

  ;; Embedded latex images are a bit small by default.
  (plist-put org-format-latex-options :scale 1.4)

  ;; Auto-save after refiling.
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  :hook ((org-capture-mode . evil-insert-state))
  :bind (("C-c c" . org-capture)
         ("C-c o" . org-agenda)
         ("C-c l" . org-store-link)))

(use-package org-crypt
  :after org
  :config
  (setq org-crypt-key "C201D03C1BB3A68E"
        org-crypt-tag-matcher "encrypt")
  (org-crypt-use-before-save-magic))

(use-package ob
  :config
  (setq org-plantuml-exec-mode 'plantuml
        org-babel-lisp-eval-fn #'sly-eval)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (emacs-lisp . t)
     (lisp       . t)
     (scheme     . t)
     (sql        . t)
     (http       . t)
     (gnuplot    . t)
     (calc       . t)
     (sql        . t)
     (python     . t)
     (R          . t)
     (plantuml   . t)))
  :hook ((org-babel-after-execute . org-display-inline-images)))

(use-package ob-http
  :straight t
  :after ob)

(use-package gnuplot
  :straight t
  :after ob)

(use-package ob-async
  :straight t
  :after ob)

(provide 'seanmacs-org)
;;; seanmacs-org.el ends here
