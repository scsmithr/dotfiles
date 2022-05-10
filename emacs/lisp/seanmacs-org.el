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
  ;; j - org-agenda-goto-date
  ;; k - org-agenda-capture
  ;; g - org-agenda-redo-all
  ;; C-w - kill-region
  (evil-define-key 'emacs 'org-agenda-mode-map
    "j" #'org-agenda-next-line
    "k" #'org-agenda-previous-line
    "gj" #'org-agenda-next-item
    "gk" #'org-agenda-previous-item
    "gr" #'org-agenda-redo-all
    (kbd "C-w") 'evil-window-map
    (kbd "S-<return>") #'org-agenda-goto
    (kbd "g TAB") #'org-agenda-goto)

  (setq org-default-notes-file (concat sm/notes-dir "refile.org")
        org-archive-location "archive/%s_archive::" ;; Keep top level directory clean.
        org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path t
        org-startup-folded nil
        org-startup-with-inline-images nil
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
        org-global-properties '(("Effort_ALL" . "5min 15min 30min 1h 2h 4h 8h 1d"))
        org-ellipsis "…")

  ;; Default except no special pdf handling. This will cause PDFs to open in
  ;; emacs.
  (setq org-file-apps '((auto-mode . emacs)
                        (directory . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)))

  (setq org-use-fast-todo-selection 'expert
        org-fast-tag-selection-single-key 'expert)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b)" "IN-PROGRESS(i)" "|" "REFILED(r)" "DONE(d)" "CANCELED(c)")))

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
            (tags "+revisit"
                  ((org-agenda-overriding-header "Revisit")
                   (org-agenda-prefix-format "%i %-12:c ")))
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
        org-show-context-detail '((default . canonical))
        org-agenda-block-separator ?-)

  (setq org-attach-store-link-p t
        org-attach-expert t
        org-attach-use-inheritance t)

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

  (defun sm/org-set-before-save-hook ()
    (add-hook 'before-save-hook #'delete-trailing-whitespace 99 t))

  :hook ((org-capture-mode . evil-insert-state)
         (org-mode . sm/org-set-before-save-hook))
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
  :after org
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
  :hook ((org-babel-after-execute . org-redisplay-inline-images)))

(use-package ob-http
  :straight t
  :after ob)

(use-package gnuplot
  :straight t
  :after ob)

(use-package ob-async
  :straight t
  :after ob)

;; Export org subtrees as github issues.

(use-package ghub
  :straight t)

(use-package ox-gfm
  :straight t
  :after org)

(defvar sm/org-github-issue-create-done-state "REFILED"
  "Move the org entry to this state after successfully creating an issue.")

(defvar sm/org-github-project-property "GH-PROJECT"
  "Property that should be used to specify the github repo to create issues in.

Should be in the form of 'owner/repo'.")

(defvar sm/org-github-issue-property "GH-ISSUE"
  "The property containing the url of the github issue. Set after issue was successfully created.")

(defun sm/org-get-github-issue-payload ()
  (let* ((title (org-get-heading t t t t))
         (org-export-with-toc nil)
         (body (org-export-as 'gfm t nil t)))
    (set-text-properties 0 (length title) nil title)
    `((title . ,title)
      (body . ,body))))

(defun sm/org-subtree-to-github-issue ()
  "Create an issue from the org entry under point.

The GH-PROJECT property must be set for the org entry or some
parent entry.

The org entry must be in a TODO state, and will be flipped to the
state is defined in `sm/org-github-issue-create-done-state'."
  (interactive)
  (unless (org-entry-is-todo-p)
    (user-error "Org entry is not a todo"))
  (let* ((org-use-property-inheritance (list sm/org-github-project-property))
         (gh-project (org-entry-get (point) sm/org-github-project-property 'selective)))
    (unless gh-project
      (user-error "Org entry missing github project property"))
    (let ((resource (format "/repos/%s/issues"
                            gh-project))
          (payload (sm/org-get-github-issue-payload)))
      (recenter) ;; Widening after narrowing often moves the beginning of the entry off screen.
      (unless (y-or-n-p (format "Create issue '%s' for '%s'? " (alist-get 'title payload) gh-project))
        (user-error "Issue creation canceled"))
      (let* ((resp (ghub-post resource nil
                              :auth 'org-export
                              :payload payload))
             (html-url (alist-get 'html_url resp))
             (issue-link (format "[[%s]]" html-url)))
        (org-todo sm/org-github-issue-create-done-state)
        (org-set-property sm/org-github-issue-property issue-link)
        (message "Created issue: %s" html-url)))))

;; Helpers for org attachments

(defun sm/org-convert-file-to-attach ()
  "Convert the file link under point to an attachment."
  (interactive)
  (require 'org-attach)
  (let ((context (org-element-context)))
    (if (not (and (eq (car context) 'link)))
        (user-error "Context not a link")
      (let ((type (org-element-property :type context))
            (path (org-element-property :path context)))
        (org-attach-attach path)
        (message "File attached")))))

(defun sm/org-attach-buffer-file ()
  "Attach the current buffer's file to a subtree in other window."
  (interactive)
  (require 'org-attach)
  (let ((start-win (selected-window))
        (other-win (get-window-with-predicate
                    (lambda (window)
                      (with-current-buffer (window-buffer window)
                        (eq major-mode 'org-mode)))))
        (path buffer-file-name))
    (unless other-win
      (user-error "No window displaying an Org buffer"))
    (unless path
      (user-error "Buffer has no file"))
    (select-window other-win)
    (org-attach-attach path)
    (select-window start-win)))

(provide 'seanmacs-org)
;;; seanmacs-org.el ends here
