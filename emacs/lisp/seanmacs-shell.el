;;; seanmacs-shell.el --- Shell -*- lexical-binding: t -*-

;;; Commentary:
;; Configurations for shell and eshell

;;; Code:

(defun seanmacs/async-shell-buffer (&rest args)
  "Run PROGRAM with ARGS async."
  (interactive)
  (let* ((command (mapconcat #'(lambda (a)
                                 (if (numberp a)
                                     (number-to-string a)
                                   a))
                             args " "))
         (buf-name (concat "*Async: " command "*")))
    (async-shell-command command buf-name)))

(defun seanmacs/listify-env-vars (env val &rest rest)
  (let ((list '()))
    (while env
      (let ((s (format "%s=%s" env val)))
        (push s list))
      (setq env (pop rest) val (pop rest)))
    list))

(defun seanmacs/append-process-environment (env val &rest rest)
  (append
   (apply 'seanmacs/listify-env-vars env val rest)
   process-environment
   '()))

(use-package shrink-path
  :straight t
  :commands (shrink-path-prompt shrink-path-dirs))

(defvar seanmacs/eshell-append-history-on-command t
  "Whether or not eshell should write to the history file before each command.")

(use-package eshell
  ;; built-in
  :init

  (defun seanmacs/add-eshell-aliases ()
    ;; Shell command aliases. I'd rather not keep track of the eshell
    ;; aliases file.
    (dolist (alias '(("cargo" "cargo --color=always $*")
                     ;; kubectl
                     ("kgp" "kubectl get pods $*")
                     ("kgs" "kubectl get service $*")
                     ("kgn" "kubectl get namespace $*")
                     ("kdp" "kubectl delete pod $*")
                     ("ksn" "kubectl config set-context --current --namespace=$1")
                     ("kcn" "kubectl config view --minify --output 'jsonpath={..namespace}'; echo")
                     ("kl" "kubectl logs $* --all-containers")))
      (add-to-list 'eshell-command-aliases-list alias)))

  (defun seanmacs/eshell-append-history ()
    "Append the most recent command in eshell's history ring to history file."
    (when (and eshell-history-ring
               seanmacs/eshell-append-history-on-command)
      (let ((newest-cmd-ring (make-ring 1)))
        (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
        (let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t)))))

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

  (defalias 'eshell/ff 'find-file)
  (defalias 'eshell/async 'seanmacs/async-shell-buffer)

  (defun eshell/d (&optional path)
    (dired (or path ".")))

  (defun eshell/mkcd (path)
    (let ((args (list "-p" path)))
      (eshell/mkdir args)
      (eshell/cd path)))

  (defun eshell/pd ()
    (if (projectile-project-p)
        (cd (projectile-project-root))
      (user-error "Not in project")))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

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

  (defun eshell/ss ()
    "Insert the previous command prepended with 'sudo'."
    (let* ((cmd (nth 1 (ring-elements eshell-history-ring)))
           (sudo (format "sudo %s" cmd)))
      (seanmacs/eshell-insert sudo)))

  (defun eshell/read-history ()
    (interactive)
    (eshell-read-history)
    (seanmacs/eshell-insert (completing-read
                             "History: "
                             (mapcar #'string-trim
                                     (delete-dups
                                      (ring-elements eshell-history-ring))))))

  (setq eshell-history-size 10000
        eshell-save-history-on-exit nil ;; This is handled elsewhere.
        eshell-cmpl-cycle-completions nil
        eshell-prompt-function #'eshell-default-prompt
        eshell-prompt-regexp "^.* > ")

  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; Needs to be ran inside the hook since eshell-mode-map is
              ;; buffer local.
              ;;
              ;; See https://github.com/noctuid/general.el/issues/80
              (local-set-key (kbd "C-c h") 'eshell/read-history)))

  :hook ((eshell-mode . seanmacs/add-eshell-aliases)
         (eshell-pre-command . seanmacs/eshell-append-history)))

(use-package shell
  :config)

(provide 'seanmacs-shell)
;;; seanmacs-shell.el ends here

