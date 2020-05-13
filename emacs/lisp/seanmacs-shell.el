;;; seanmacs-shell.el --- Shell -*- lexical-binding: t -*-

;;; Commentary:
;; Configurations for shell and eshell

;;; Code:

(defun async-shell-buffer (program &rest args)
  "Run PROGRAM with ARGS async."
  (interactive)
  (let* ((command (string-join (append (list program) args) " "))
         (output-buffer (concat "*" command "*")))
    (async-shell-command command output-buffer)))

(use-package shrink-path
  :straight t
  :commands (shrink-path-prompt shrink-path-dirs))

(use-package eshell
  ;; built-in
  :init
  (defun seanmacs/add-eshell-aliases ()
    ;; Shell command aliases. I'd rather not keep track of the eshell
    ;; aliases file.
    (dolist (alias '(("cargo" "cargo --color=always $*")))
      (add-to-list 'eshell-command-aliases-list alias)))
  :config
  (defface eshell-prompt-pwd '((t :inherit font-lock-constant-face))
    "Face for current directory."
    :group 'eshell)

  (defface eshell-prompt-short-pwd '((t :inherit font-lock-comment-face))
    "Face for shortened path."
    :group 'eshell)

  (defface eshell-prompt-git-branch '((t :inherit font-lock-builtin-face))
    "Face for displaying current git branch."
    :group 'eshell)

  (defun eshell--current-git-branch ()
    (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
                                if (string-match-p "^\*" match)
                                collect match))))
      (if (not (eq branch nil))
          (format " %s" (substring branch 2))
        "")))

  (defun eshell-default-prompt ()
    "Generate the prompt string for eshell.  Use for `eshell-prompt-function'."
    (let ((base/dir (shrink-path-prompt default-directory)))
      (concat (propertize (car base/dir)
                          'face 'eshell-prompt-short-pwd)
              (propertize (cdr base/dir)
                          'face 'eshell-prompt-pwd)
              (unless (file-remote-p default-directory)
                (propertize (eshell--current-git-branch)
                            'face 'eshell-prompt-git-branch))
              (propertize " >" 'face (if (zerop eshell-last-command-status) 'success 'error))
              ;; needed for the input text to not have prompt face
              (propertize " " 'face 'default))))

  (defun eshell-new ()
    (interactive)
    (eshell "new"))

  (defalias 'eshell/ff 'find-file-other-window)
  (defalias 'eshell/async 'async-shell-buffer)

  (defun eshell/d (&optional path)
    (dired-other-window (or path ".")))

  (defun eshell/mkcd (path)
    (let ((args (list "-p" path)))
      (eshell/mkdir args)
      (eshell/cd path)))

  (defun eshell/pd ()
    (if (projectile-project-p)
        (cd (projectile-project-root))
      (user-error "Not in project")))

  (defun seanmacs/eshell-insert (&rest args)
    (goto-char (point-max))
    ;; Only reset prompt if there's already some input.
    (let ((curr-point (point)))
      (eshell-bol)
      (let ((no-input (eq (- curr-point (point)) 0)))
        (unless no-input
          (eshell-reset))))
    (evil-insert 1)
    (apply 'insert args))

  (defun eshell/read-history ()
    (interactive)
    (seanmacs/eshell-insert (completing-read
                             "History: "
                             (mapcar #'string-trim
                                     (delete-dups
                                      (ring-elements eshell-history-ring))))))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun seanmacs/eshell-find-subdirectory-recursive (dir)
    (let* ((contents (directory-files-recursively dir ".*" t))
           (find-directories (mapcar (lambda (x)
                                       (when (file-directory-p x)
                                         (abbreviate-file-name x)))
                                     contents))
           (subdirs (delete nil find-directories))
           (cands (cl-remove-if (lambda (x)
                                  (or
                                   (string-match-p "\\\\_target" x)
                                   (string-match-p "\\vendor" x)
                                   (string-match-p "\\node_modules" x)
                                   (string-match-p "\\.git" x)))
                                subdirs))
           (selection (completing-read "Find sub-directory: " cands nil t)))
      (seanmacs/eshell-insert selection)
      (eshell-send-input)))

  (defun eshell/find-subdirectory-pwd ()
    (interactive)
    (seanmacs/eshell-find-subdirectory-recursive (eshell/pwd)))

  (defun eshell/find-subdirectory-project ()
    (interactive)
    (if (projectile-project-p)
        (seanmacs/eshell-find-subdirectory-recursive (projectile-project-root))
      (user-error "Not in project")))

  (setq eshell-history-size 1000
        eshell-cmpl-cycle-completions nil
        eshell-prompt-function #'eshell-default-prompt
        eshell-prompt-regexp "^.* > ")

  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; Needs to be ran inside the hook since eshell-mode-map is
              ;; buffer local.
              ;;
              ;; See https://github.com/noctuid/general.el/issues/80
              (local-set-key (kbd "C-c h") 'eshell/read-history)
              (local-set-key (kbd "C-c d") 'eshell/find-subdirectory-pwd)
              (local-set-key (kbd "C-c p") 'eshell/find-subdirectory-project)))

  :hook ((eshell-mode . seanmacs/add-eshell-aliases)))

(use-package shell
  :config)

(provide 'seanmacs-shell)
;;; seanmacs-shell.el ends here

