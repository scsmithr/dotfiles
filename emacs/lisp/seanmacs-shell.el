;;; seanmacs-shell.el --- Shell -*- lexical-binding: t -*-

;;; Commentary:
;; Configurations for shell and eshell

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'subr-x)

(defun sm/compile (command &optional buf-name)
  "Run COMMAND in a compilation buffer named BUF-NAME.

If BUF-NAME is nil, the command will be used to name the buffer."
  (interactive (list (compilation-read-command "")))
  (let ((compilation-buffer-name-function
         #'(lambda (_mode) (or buf-name (format "*Compile: %s*" command)))))
    (compile command)))

(defun sm/buffer-setenv (env val)
  "Set an environment variable for the buffer."
  (interactive (list (read-string "Environment variable: ")
                     (read-string "Value: ")))
  (make-local-variable 'process-environment)
  (unless (or (string-empty-p env)
              (string-empty-p val))
    (setq process-environment (cons (format "%s=%s" env val)
                                    process-environment))))

(defun sm/buffer-kill-env ()
  "Remove the locally set environment."
  (interactive)
  (when (local-variable-p 'process-environment)
    (kill-local-variable 'process-environment)))

(use-package compile
  ;; built-in
  :init
  (require 'ansi-color)
  (defun sm/colorize-compile-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (add-to-list 'compilation-error-regexp-alist-alist
               '(rust-error
                 "^error\\[E[0-9]+\\]:.*\n[ ]*--> \\(.*\\):\\([0-9]+\\):\\([0-9]+\\)"
                 1 2 3))

  (add-to-list 'compilation-error-regexp-alist 'rust-error)

  (setq compilation-scroll-output t)

  :hook ((compilation-filter . sm/colorize-compile-buffer)))

(defvar sm/eshell-append-history-on-command t
  "Whether or not eshell should write to the history file before each command.")

(defvar sm/eshell-aliases
  '(
    ;; kubectl
    ("kgp" "kubectl get pods $*")
    ("kgs" "kubectl get service $*")
    ("kgn" "kubectl get namespace $*")
    ("kdp" "kubectl delete pod $*")
    ("ksn" "kubectl config set-context --current --namespace=$1")
    ("kcn" "kubectl config view --minify --output 'jsonpath={..namespace}'; echo")
    ("kl" "kubectl logs $* --all-containers")

    ("clear" "clear-scrollback"))
  "Custom eshell aliases.")

(use-package eshell
  ;; built-in
  :init
  (defun sm/add-eshell-aliases ()
    (dolist (alias sm/eshell-aliases)
      (add-to-list 'eshell-command-aliases-list alias)))

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
    (if-let (project (project-current))
        (eshell/cd (project-root project))
      (user-error "Not in project")))

  ;; Nearly identical to the default prompt function, but will display last
  ;; status if non-zero.
  (defun sm/eshell-prompt-function ()
    (concat (abbreviate-file-name (eshell/pwd))
            (unless (zerop eshell-last-command-status)
              (format " [%s]" eshell-last-command-status))
            (if (= (user-uid) 0) " # " " % ")))

  (setq eshell-prompt-function #'sm/eshell-prompt-function
        eshell-prompt-regexp "^.* [#%] ")

  (setq eshell-cmpl-cycle-completions nil
        eshell-banner-message
        '(format "%s %s\n\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'bold)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)))

  ;; When using eat, programs that would normally go in this list just work.
  (setq eshell-visual-commands nil)

  (defun sm/eshell-add-completions ()
    (when (featurep 'cape)
      (add-to-list 'completion-at-point-functions #'cape-file)))

  (defun sm/eshell-append-history ()
    "Append the most recent command in eshell's history ring to history file."
    (when eshell-history-ring
      (let ((newest-cmd-ring (make-ring 1)))
        (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
        (let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t)))))

  (setq eshell-save-history-on-exit nil) ;; This is handled by `sm/eshell-append-history'.

  ;; Remove hook that prevents the cursor from going back after hitting escape.
  ;; This throws me off so much.
  ;;
  ;; Added in https://github.com/emacs-evil/evil-collection/commit/a81b6c8f5537b3646e6a66a6e60ec634848d1926
  (remove-hook 'eshell-mode-hook 'evil-collection-eshell-escape-stay)

  ;; Expand !<n> and !!
  (add-hook 'eshell-expand-input-functions #'eshell-expand-history-references)

  :hook ((eshell-mode . sm/add-eshell-aliases)
         (eshell-pre-command . sm/eshell-append-history)
         (eshell-mode . sm/eshell-add-completions)
         (eshell-mode . eat-eshell-mode))
  :bind (:map shell-prefix-map
              ("s" . eshell)
              ("n" . sm/eshell-new)))

(use-package em-hist
  ;; built-in
  :init
  (setq eshell-history-size (* 10 1048)
        eshell-hist-ignoredups t)

  (defun sm/eshell-consult-history ()
    "Put eshell into insert mode prior to consulting history."
    (interactive)
    (goto-char (point-max))
    (eshell-bol)
    (evil-insert 1)
    (consult-history))

  :bind (:map eshell-hist-mode-map
              ("C-c C-l" . sm/eshell-consult-history)))

(use-package term
  ;; built in
  :bind (:map shell-prefix-map
              ("t" . ansi-term)))

(use-package eat
  :straight (:type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(provide 'seanmacs-shell)
;;; seanmacs-shell.el ends here

