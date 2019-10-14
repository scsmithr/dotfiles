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
   "dd" 'dired)
  :config
  (setq dired-listing-switches "-aBhl --group-directories-first"))

(use-package help
  :config
  (shackle '(("^\\*Help\\*$" :height 0.3))))

(use-package gcloud
  :straight (gcloud :type git :host github :repo "scsmithr/gcloud.el")
  :init
  (core/leader
   "ags" 'gcloud-instance-shell
   "age" 'gcloud-instance-eshell))

(provide 'seanmacs-utils)
;;; seanmacs-utils.el ends here
