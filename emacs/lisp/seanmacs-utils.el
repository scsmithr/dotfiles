;;; seanmacs-utils.el --- Utils -*- lexical-binding: t; -*-

;;; Commentary:
;; Other utilities.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package epg
  ;; built-in
  :init
  (setq auth-sources '("~/.authinfo.gpg" "~/.netrc.gpg")
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
  :bind (("C-c a d" . docker)))

(use-package docker-tramp
  :straight t)

(use-package restclient
  :straight t)

(use-package dired
  ;; built-in
  :config
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

  (defun sm/dired-maybe-insert-subdir (dirnames)
    "Insert subdirs for all DIRNAMES.

When called interactively, DIRNAMES will be all currently marked
files in the current dired buffer. If no files are marked, the
file at point will be used."
    (interactive (list (dired-get-marked-files)))
    (dolist (dirname dirnames)
      (dired-maybe-insert-subdir dirname)))

  (defun sm/dired-preview-file-other-window ()
    "Open file under point in other window."
    (interactive)
    (when (eq major-mode 'dired-mode)
      (sm/save-window-excursion
       (find-file-other-window (dired-get-filename)))))

  (evil-collection-define-key 'normal 'dired-mode-map
    "K" #'dired-kill-subdir
    "I" #'sm/dired-maybe-insert-subdir
    "gj" #'dired-next-subdir
    "gk" #'dired-prev-subdir
    "]]" #'dired-next-subdir
    "[[" #'dired-prev-subdir
    (kbd "TAB") #'dired-hide-subdir
    (kbd "SPC") #'sm/dired-preview-file-other-window)

  (defun sm/dired-with-current-directory (orig-fn &rest args)
    "Set `default-directory' to dired's current dir if in dired mode."
    (if (eq major-mode 'dired-mode)
        (let ((default-directory (dired-current-directory)))
          (apply orig-fn args))
      (apply orig-fn args)))

  ;; Make find-file subdir aware.
  (advice-add 'find-file-read-args :around 'sm/dired-with-current-directory)

  :hook ((dired-mode . diredfl-mode))
  :bind (("C-x d" . dired-jump)
         :map dired-mode-map
         ("C-c C-n" . dired-next-subdir)
         ("C-c C-p" . dired-prev-subdir)))

(use-package diredfl
  :straight t)

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; Sharper text on macos.
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  (defvar sm/pdf-to-text-fill t
    "Determine if the plain text should auto filled.")

  (defvar sm/pdf-to-text-command "pdftotext"
    "Path to command for converting a pdf file to plain text.")

  (defun sm/pdf-to-text (file)
    "Convert a pdf file to plain text.

The output will be put into a temporary buffer."
    (interactive (list (buffer-file-name)))
    (let* ((file (concat "\"" file "\""))
           (cmd (string-join (list sm/pdf-to-text-command file "-") " "))
           (output-buf (format "%s [text]" (file-name-nondirectory file))))
      (message "cmd: %s" cmd)
      (shell-command cmd output-buf)
      (switch-to-buffer-other-window output-buf)
      (text-mode)
      (when sm/pdf-to-text-fill
        (fill-region (point-min) (point-max))
        (set-buffer-modified-p nil))))

  (defun sm/pdf-outline-show-link ()
    "Show link in pdf window, keeping focus in the outline."
    (interactive)
    (sm/save-window-excursion
     (call-interactively 'pdf-outline-follow-link)))

  (evil-collection-define-key 'normal 'pdf-outline-buffer-mode-map
    (kbd "SPC") #'sm/pdf-outline-show-link)

  :bind (:map pdf-view-mode-map
              ("C-c C-t" . sm/pdf-to-text)))

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

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))

(use-package olivetti
  :straight t
  :config
  (setq olivetti-body-width 92)
  :bind (("C-c z" . zen-mode)))

(defvar-local zen-mode-display-line-numbers-mode nil
  "Non-nil if the `display-line-numbers-mode' was enabled.")

(define-minor-mode zen-mode
  "Mode that centers the current buffer and disables line numbers."
  :init-value nil
  :global nil
  :lighter "Zen"
  (if zen-mode
      (progn
        (when (bound-and-true-p display-line-numbers-mode)
          (setq zen-mode-display-line-numbers-mode t)
          (display-line-numbers-mode -1))
        (olivetti-mode 1))
    (progn
      (when zen-mode-display-line-numbers-mode
        (display-line-numbers-mode 1))
      (olivetti-mode -1))))

(provide 'seanmacs-utils)
;;; seanmacs-utils.el ends here
