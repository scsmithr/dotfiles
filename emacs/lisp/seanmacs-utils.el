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
   "ad" 'docker))

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

(use-package help
  :config)

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
   "ak" 'kube))

(provide 'seanmacs-utils)
;;; seanmacs-utils.el ends here
