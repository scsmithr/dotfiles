;;; seanmacs-utils.el --- Utils -*- lexical-binding: t; -*-

;;; Commentary:
;; Other utilities.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package epg
  ;; built-in
  :init
  (setq auth-sources `(,(concat sm/sync-dir "authinfo.gpg") "~/.authinfo.gpg")
        epg-pinentry-mode 'loopback))

(use-package tabulated-list
  ;; built-in
  :init
  (setq tabulated-list-gui-sort-indicator-asc ?↓
        tabulated-list-gui-sort-indicator-desc ?↑))

(use-package shr
  ;; builtin
  :config
  (setq shr-width 80
        shr-max-image-proportion 0.7))

(use-package docker
  :straight t
  :config
  (setq docker-container-columns
        '((:name "Id" :width 16 :template "{{json .ID}}" :sort nil :format nil)
          (:name "Names" :width 24 :template "{{json .Names}}" :sort nil :format nil)
          (:name "Status" :width 16 :template "{{json .Status}}" :sort nil :format
                 (lambda
                   (x)
                   (propertize x 'font-lock-face
                               (docker-container-status-face x))))
          (:name "Image" :width 16 :template "{{json .Image}}" :sort nil :format nil)
          (:name "Command" :width 30 :template "{{json .Command}}" :sort nil :format nil)
          (:name "Created" :width 23 :template "{{json .CreatedAt}}" :sort nil :format
                 (lambda
                   (x)
                   (format-time-string "%F %T"
                                       (date-to-time x))))
          (:name "Ports" :width 10 :template "{{json .Ports}}" :sort nil :format nil)))
  :bind (:map app-prefix-map
              ("d" . docker)))

(use-package dired
  ;; built-in
  :config
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
        dired-dwim-target t)

  (defun sm/dired-maybe-insert-subdir (dirnames)
    "Insert subdirs for all DIRNAMES.

When called interactively, DIRNAMES will be all currently marked
files in the current dired buffer. If no files are marked, the
file at point will be used."
    (interactive (list (dired-get-marked-files)))
    (dolist (dirname dirnames)
      (dired-maybe-insert-subdir dirname)))

  (evil-collection-define-key 'normal 'dired-mode-map
    "K" #'dired-kill-subdir
    "I" #'sm/dired-maybe-insert-subdir
    "gj" #'dired-next-subdir
    "gk" #'dired-prev-subdir
    "]]" #'dired-next-subdir
    "[[" #'dired-prev-subdir
    (kbd "TAB") #'dired-hide-subdir)

  (defun sm/dired-with-current-directory (orig-fn &rest args)
    "Set `default-directory' to dired's current dir if in dired mode."
    (if (eq major-mode 'dired-mode)
        (let ((default-directory (dired-current-directory)))
          (apply orig-fn args))
      (apply orig-fn args)))

  ;; Make find-file subdir aware.
  (advice-add 'find-file-read-args :around 'sm/dired-with-current-directory)

  :hook ((dired-mode . diredfl-mode))
  :bind (("C-x d" . dired-jump)))

(use-package diredfl
  :straight t)

(use-package doc-view
  ;; built-in
  :init

  ;; Disable highlight line specifically so it doesn't show up when rendering
  ;; PDFs as SVGs.
  (defun sm/doc-view-disable-hl-line ()
    (setq-local global-hl-line-mode nil))

  :config

  (setq doc-view-mupdf-use-svg t)

  :hook ((doc-view-mode . sm/doc-view-disable-hl-line)))

(use-package ffap
  ;; built-in
  :demand t
  :init
  (defvar sm/ffap-disable-for-modes
    '(dired-mode) ;; I use C-x C-f exclusively to create files in dired.
    "List of major modes where `ffap' should not be used.")

  (defun sm/find-file ()
    "Dispatch to `find-file' and `ffap' as appropriate.

If file name under point contains a line number, jump to it after
opening the file."
    (interactive)
    (if (member major-mode sm/ffap-disable-for-modes)
        (call-interactively 'find-file)
      (let* ((str (ffap-string-at-point))
             (idx (string-match ":[0-9]+$" str))
             (file (if idx (substring str 0 idx) str))
             (line (when (and idx file) (substring str (+ 1 idx)))))
        (call-interactively 'find-file-at-point)
        (when line
          (goto-line (string-to-number line))
          (recenter nil)))))

  :bind (("C-x C-f" . sm/find-file)))

(use-package help
  ;; built-in
  :config
  (evil-collection-define-key 'normal 'help-mode-map
    "p" #'sm/pop-to-some-window))

(use-package tramp-sh
  ;; built-in
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-ssh-controlmaster-options (concat "-o ControlMaster=auto "
                                                "-o ControlPath='tramp.%%C' "
                                                "-o ControlPersist=yes")))

(use-package tramp
  ;; built-in
  :config
  (setq tramp-verbose 3))

(use-package proced
  ;; built-in
  :config
  (setq proced-enable-color-flag t))

(provide 'seanmacs-utils)
;;; seanmacs-utils.el ends here
