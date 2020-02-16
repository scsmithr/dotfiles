;;; seanmacs-utils.el --- Utils -*- lexical-binding: t; -*-

;;; Commentary:
;; Other utilities.

;;; Code:

(use-package docker
  :straight t
  :defer t
  :commands (docker)
  :init
  (core/leader
   "ad" 'docker)
  (shackle '(("^\\*docker"
              :action seanmacs/display-buffer-bottom
              :height 0.3))))

(use-package docker-tramp
  :straight t
  :defer t)

(use-package restclient
  :straight t)

(use-package dired
  :init
 (core/leader
   "dd" 'dired
   "df" 'find-file)
  :config
  (setq dired-listing-switches "-Ahlv --group-directories-first")
  (advice-add 'dired-up-directory :around #'seanmacs/run-and-bury)
  (advice-add 'dired-find-file :around #'seanmacs/run-and-bury))

(use-package dired-subtree
  :straight t
  :config
  (defun seanmacs/dired-refresh-icons ()
    (revert-buffer))
  (advice-add 'dired-subtree-toggle :after #'seanmacs/dired-refresh-icons))

(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar
             dired-sidebar-show-sidebar
             dired-sidebar-jump-to-sidebar)
  :init
  (core/leader
   "tt" 'seanmacs/jump-to-sidebar
   "tn" 'dired-sidebar-toggle-sidebar)
  (defun seanmacs/jump-to-sidebar ()
    (interactive)
    (dired-sidebar-show-sidebar)
    (dired-sidebar-follow-file)
    (dired-sidebar-jump-to-sidebar))
  :config
  (setq dired-sidebar-theme 'icons
        dired-sidebar-width 24
        dired-sidebar-should-follow-file nil))

(use-package help
  :config
  (shackle '(("^\\*Help\\*$"
              :height 0.3)
             ("^\\*Completions\\*$"
              :action display-buffer-below-selected
              :height 0.2))))

(use-package gcloud
  :straight (gcloud :type git :host github :repo "scsmithr/gcloud.el")
  :init
  (core/leader
   "ags" 'gcloud-instance-shell
   "age" 'gcloud-instance-eshell))

(use-package kube
  :straight (kube :type git :host github :repo "scsmithr/kube.el")
  :init
  (core/leader
   "ak" 'kube)
  (shackle '(("^\\*kube\\*"
              :action seanmacs/display-buffer-bottom
              :height 0.3)))
  (shackle '(("^\\*kube "
              :action seanmacs/display-buffer-same))))

(provide 'seanmacs-utils)
;;; seanmacs-utils.el ends here
