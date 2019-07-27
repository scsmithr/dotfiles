;;; orgmode.el --- Org related stuff

(core/leader "x x" 'org-capture)

;; Where org-captures go.
(setq org-default-notes-file "~/notes/refile.org")

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

