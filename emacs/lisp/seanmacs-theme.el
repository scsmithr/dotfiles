;;; seanmacs-theme.el --- Theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme stuff.

;;; Code:

(defun face-attr (face &rest args)
  (apply #'set-face-attribute face nil args))

(defun reset-face (face)
  (apply #'face-spec-reset-face face nil))

;; Default font
(let ((default-monospace "Fira Mono"))
  (set-frame-font (font-spec :family default-monospace))
  (face-attr 'default :font default-monospace :height 110))
(face-attr 'variable-pitch :family "Fira Sans" :height 110)

(use-package modus-operandi-theme
  :straight (modus-operandi-theme :type git :host github :repo "scsmithr/modus-themes")
  :config
  (setq modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-rainbow-headings t)
  (load-theme 'modus-operandi t))

(use-package all-the-icons
  :straight t
  :config
  (setq all-the-icons-color-icons t
        all-the-icons-scale-factor 1.0
        all-the-icons-default-adjust 0.0)
  (setq all-the-icons-icon-alist
        '(
          ("\\.md$" all-the-icons-octicon "file-text")
          ("\\.org$" all-the-icons-octicon "file-text")
          ("\\.txt$" all-the-icons-octicon "file-text")
          ("\\.log$" all-the-icons-octicon "file-text")
          ("\\.rst$" all-the-icons-octicon "file-text")
          ("\\.o$" all-the-icons-octicon "file-binary")
          ("\\.exe$" all-the-icons-octicon "file-binary")
          ("\\.so$" all-the-icons-octicon "file-binary")
          ("\\.out$" all-the-icons-octicon "file-binary")
          ("\\.pdf$" all-the-icons-octicon "file-pdf")
          ("\\.zip$" all-the-icons-octicon "file-zip")
          ("\\.tar$" all-the-icons-octicon "file-zip")
          ("\\.tgz$" all-the-icons-octicon "file-zip")
          ("\\.gz$" all-the-icons-octicon "file-zip")
          ("\\.jpg$" all-the-icons-octicon "file-media")
          ("\\.jpeg$" all-the-icons-octicon "file-media")
          ("\\.png$" all-the-icons-octicon "file-media")
          ("\\.gif$" all-the-icons-octicon "file-media")
          ("\\.svg$" all-the-icons-octicon "file-media")
          ("\\.mkv$" all-the-icons-octicon "file-media")
          ("\\.mp3$" all-the-icons-octicon "file-media")
          ("\\.mp4$" all-the-icons-octicon "file-media")
          ("\\.ogg$" all-the-icons-octicon "file-media")
          ("\\.midi$" all-the-icons-octicon "file-media")
          ("." all-the-icons-octicon "file-code")))
  (setq all-the-icons-dir-icon-alist
        '(
          ("." all-the-icons-octicon "file-directory"))))

(use-package all-the-icons-dired
  :straight t
  :if window-system
  :config
  (setq all-the-icons-dired-v-adjust 0.1)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'seanmacs-theme)
;;; seanmacs-theme.el ends here
