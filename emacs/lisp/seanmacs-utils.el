;;; seanmacs-utils.el --- Utils -*- lexical-binding: t; -*-

;;; Commentary:
;; Other utilities.

;;; Code:

;; Quickly generate lang formatters.
(use-package reformatter
  :straight t)

(use-package docker
  :straight t
  :defer t
  :commands (docker))

(use-package docker-tramp
  :straight t
  :defer t)

(use-package restclient
  :straight t)

(use-package dired
  :config
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  :hook ((dired-mode . diredfl-mode)))

(use-package diredfl
  :straight t
  :defer t)

(use-package dired-narrow
  :straight t
  :config
  (evil-collection-define-key 'normal 'dired-mode-map "/" 'dired-narrow))

(use-package dired-open
  :straight t
  :config
  (setq dired-open-extensions '(("pdf" . "zathura"))))

(use-package help
  :config)

(use-package gcloud
  :straight (gcloud :type git :host github :repo "scsmithr/gcloud.el"))

(use-package kube
  :straight (kube :type git :host github :repo "scsmithr/kube.el"))

(provide 'seanmacs-utils)
;;; seanmacs-utils.el ends here
