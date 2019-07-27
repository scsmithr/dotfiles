;;; orgmode.el --- Org related stuff

;; Where org-captures go.
(setq org-default-notes-file "~/notes/refile.org")

(global-set-key (kbd "C-c c") 'org-capture)

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

