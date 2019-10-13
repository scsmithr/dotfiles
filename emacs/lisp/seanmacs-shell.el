;;; seanmacs-shell.el --- Shell -*- lexical-binding: t -*-

;;; Commentary:
;; Configurations for shell and eshell

;;; Code:

(defun disable-completions-tramp ()
  "Disable completions if buffer is remote."
  (when (file-remote-p default-directory) (company-mode -1)))

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
  :init
  (core/leader
   "ss" 'eshell
   "sn" 'eshell-new)
  (shackle '((eshell-mode
              :action seanmacs/display-buffer-bottom
              :height 0.2)
             ("^\\*eshell"
              :action seanmacs/display-buffer-bottom
              :height 0.2)))
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
          (format " (%s)" (substring branch 2))
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

  (defalias 'eshell/d 'dired-other-window)
  (defalias 'eshell/ff 'find-file-other-window)
  (defalias 'eshell/async 'async-shell-buffer)

  (defun eshell/read-history ()
    (interactive)
    (goto-char (point-max))
    ;; Only reset prompt if there's already some input.
    (let ((curr-point (point)))
      (eshell-bol)
      (let ((no-input (eq (- curr-point (point)) 0)))
        (unless no-input
          (eshell-reset))))
    (evil-insert 1)
    (insert (ido-completing-read
             "History: "
             (mapcar #'string-trim
                     (delete-dups
                      (ring-elements eshell-history-ring))))))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (add-hook 'eshell-mode-hook #'disable-completions-tramp)
  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; Needs to be ran inside the hook since eshell-mode-map is
              ;; buffer local.
              ;;
              ;; See https://github.com/noctuid/general.el/issues/80
              (local-set-key (kbd "C-c h") 'eshell/read-history)))

  (setq eshell-history-size 1000
        eshell-cmpl-cycle-completions nil
        eshell-prompt-function #'eshell-default-prompt
        eshell-prompt-regexp "^.* > "))

(use-package shell
  :config
  (shackle '((shell-mode
              :action seanmacs/display-buffer-bottom
              :height 0.2)
             ("^\\*shell"
              :action seanmacs/display-buffer-bottom
              :height 0.2)))
  (add-hook 'shell-mode-hook #'disable-completions-tramp))

(provide 'seanmacs-shell)
;;; seanmacs-shell.el ends here

