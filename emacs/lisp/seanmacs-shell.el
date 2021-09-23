;;; seanmacs-shell.el --- Shell -*- lexical-binding: t -*-

;;; Commentary:
;; Configurations for shell and eshell

;;; Code:

(require 'subr-x)

(defun sm/compile (command &optional buf-name)
  "Run COMMAND in a compilation buffer named BUF-NAME.

If BUF-NAME is nil, the command will be used to name the buffer."
  (interactive (list (compilation-read-command "")))
  (let ((compilation-buffer-name-function
         #'(lambda (_mode) (or buf-name (format "*Compile: %s*" command)))))
    (compile command)))

(defun sm/listify-env-vars (env val &rest rest)
  "Create a list of environment variables and values suitable to use in the shell."
  (let ((list '()))
    (while env
      (let ((s (format "%s=%s" env val)))
        (push s list))
      (setq env (pop rest) val (pop rest)))
    list))

(defun sm/append-process-environment (env val &rest rest)
  (append (apply 'sm/listify-env-vars env val rest) process-environment '()))

(use-package compile
  ;; built-in
  :init
  (require 'ansi-color)
  (defun sm/colorize-compile-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (setq compilation-scroll-output 'first-error)
  :hook ((compilation-filter . sm/colorize-compile-buffer)))

(defun sm/disable-company ()
  (company-mode -1))

(defvar sm/eshell-append-history-on-command t
  "Whether or not eshell should write to the history file before each command.")

(use-package eshell
  ;; built-in
  :init
  (defun sm/add-eshell-aliases ()
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

  (defun sm/eshell-append-history ()
    "Append the most recent command in eshell's history ring to history file."
    (when (and eshell-history-ring
               sm/eshell-append-history-on-command)
      (let ((newest-cmd-ring (make-ring 1)))
        (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
        (let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t)))))

  :config
  (defun sm/eshell-new ()
    (interactive)
    (eshell "new"))

  (defalias 'eshell/ff 'find-file)

  (defun eshell/d (&optional path)
    (dired (or path ".")))

  (defun eshell/mkcd (path)
    (let ((args (list "-p" path)))
      (eshell/mkdir args)
      (eshell/cd path)))

  (defun eshell/pd ()
    (if (projectile-project-p)
        (eshell/cd (projectile-project-root))
      (user-error "Not in project")))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun sm/eshell-insert (&rest args)
    (goto-char (point-max))
    ;; Only reset prompt if there's already some input.
    (let ((curr-point (point)))
      (eshell-bol)
      (let ((no-input (eq (- curr-point (point)) 0)))
        (unless no-input
          (eshell-reset))))
    (evil-insert 1)
    (apply 'insert args))

  (defun eshell/insert-history ()
    "Prompt for a history item and insert it."
    (interactive)
    (eshell-read-history)
    (sm/eshell-insert (completing-read
                       "History: "
                       (mapcar #'string-trim
                               (delete-dups
                                (ring-elements eshell-history-ring))))))

  ;; Nearly identical to the default prompt function, but will display last
  ;; status if non-zero. Satisfies default prompt regex.
  (defun sm/eshell-prompt-function ()
    (concat (abbreviate-file-name (eshell/pwd))
            (unless (zerop eshell-last-command-status)
              (format " [%s]" eshell-last-command-status))
            (if (= (user-uid) 0) " # " " $ ")))

  (setq eshell-prompt-function #'sm/eshell-prompt-function)

  (setq eshell-history-size 10000
        eshell-save-history-on-exit nil ;; This is handled elsewhere.
        eshell-cmpl-cycle-completions nil
        eshell-banner-message "Mistake Not...\n\n")

  ;; Expand !<n> and !!
  (add-hook 'eshell-expand-input-functions #'eshell-expand-history-references)

  (defun sm/eshell-set-local-keybinds ()
    ;; Needs to be ran inside the hook since eshell-mode-map is
    ;; buffer local.
    ;;
    ;; See https://github.com/noctuid/general.el/issues/80
    (local-set-key (kbd "M-r") 'eshell/insert-history))

  :hook ((eshell-mode . sm/add-eshell-aliases)
         (eshell-mode . sm/disable-company)
         (eshell-mode . sm/eshell-set-local-keybinds)
         (eshell-pre-command . sm/eshell-append-history))
  :bind (("C-c s s" . eshell)
         ("C-c s n" . sm/eshell-new)))

(use-package shell
  ;; built-in
  :config
  :hook ((shell-mode . sm/disable-company)))

(provide 'seanmacs-shell)
;;; seanmacs-shell.el ends here

