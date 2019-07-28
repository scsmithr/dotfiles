;;; orgmode.el --- Org related stuff

(core/leader "x x" 'org-capture)

;; Where org-captures go.
(setq org-default-notes-file "~/notes/refile.org")

(setq org-agenda-files (list "~/notes/"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

;; Make refile use ido.
(setq org-outline-path-complete-in-steps nil)
(defun completion-use-ido ()
  "Set the current buffer's completing read engine to IDO."
  (setq-local completing-read-function #'ido-completing-read))
(add-hook 'org-mode-hook 'completion-use-ido)

(setq org-completion-use-ido t)

(setq org-template-directory "~/.emacs.d/org-templates")

(setq org-capture-templates
      `(
        ("n" "Note" entry (file+headline "" "Notes")
         (file ,(concat org-template-directory "/note")))
        ("t" "Task" entry (file+headline "" "Tasks")
         (file ,(concat org-template-directory "/task")))))

(setq org-startup-folded nil)
(setq org-hide-leading-stars t)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "CANCELED")))

(setq org-blank-before-new-entry (quote ((heading . always) (plain-list-item . always))))

(setq org-enforce-todo-dependencies t)

(after! org
        (org-babel-do-load-languages
         'org-babel-load-languages
         (append org-babel-load-languages
                 '((shell . t)
                   (restclient . t)
                   (http . t)))))

(after! org
        (face-attr 'org-done :foreground (doom-color 'green))
        (face-attr 'org-todo :foreground (doom-color 'yellow)))

(use-package ob-restclient
  :ensure t)

(use-package ob-http
  :ensure t)

(provide 'orgmode)
;;; orgmode.el ends here

