;;; orgmode.el --- Org related stuff

(core/leader "x x" 'org-capture)

;; Where org-captures go.
(setq org-default-notes-file "~/notes/refile.org")

(setq org-agenda-files (list "~/notes/"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

(setq org-capture-templates
      '(
        ("n" "Note" entry (file+headline "" "Notes")
         "* %?
  %U
  %i
  %a"
         )
        ("t" "Task" entry (file+headline "" "Tasks")
         "* TODO %?
  %U
  %a"
         )
        ))


(setq org-startup-folded nil)
(setq org-hide-leading-stars t)

(after! org
        (org-babel-do-load-languages
         'org-babel-load-languages
         (append org-babel-load-languages
                 '((shell . t)
                   (restclient . t)
                   (http . t)))))

(use-package ob-restclient
  :ensure t)

(use-package ob-http
  :ensure t)

(provide 'orgmode)
;;; orgmode.el ends here

