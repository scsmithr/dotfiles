;;; utils.el --- Utilities -*- lexical-binding: t; -*-

;; Auto detect indentation type/level
(use-package dtrt-indent
  :straight t
  :init
  (setq dtrt-indent-min-quality 65.0)
  (setq dtrt-indent-min-hard-tab-superiority 180.0)
  :config
  (add-to-list 'dtrt-indent-hook-mapping-list
               '(web-mode javascript web-mode-code-indent-offset))
  (dtrt-indent-global-mode 1))

;; Projectile
(use-package projectile
  :straight t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode +1)
  (core/leader
   "p" 'projectile-command-map))

(use-package ripgrep
  :straight t
  :config
  (face-attr 'ripgrep-match-face :foreground (doom-color 'yellow)))

(use-package flycheck
  :straight t
  :config
  (add-hook 'typescript-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'go-mode 'flycheck-mode)
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-indication-mode 'right-fringe)
  (core/leader "f" flycheck-command-map)
  (face-attr 'flycheck-fringe-info
             :foreground (doom-transparentize 'green 0.5)
             :background (doom-transparentize 'green 0.5))
  (face-attr 'flycheck-fringe-warning
             :foreground (doom-transparentize 'orange 0.5)
             :background (doom-transparentize 'orange 0.5))
  (face-attr 'flycheck-fringe-error
             :foreground (doom-transparentize 'red 0.5)
             :background (doom-transparentize 'red 0.5)))

(use-package magit
  :straight t
  :defer t
  :config
  (core/leader
   "gg" 'magit-status
   "gf" 'magit-file-dispatch))

(use-package forge
  :straight t
  :after magit
  :init
  (setq forge-pull-notifications nil))

(use-package evil-magit
  :straight t)

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

(use-package docker
  :straight t
  :defer t
  :commands (docker)
  :init
  (core/leader
   "ad" 'docker))

(after! docker
        ;; Unbind conflicting keybinds
        (define-key tablist-minor-mode-map (kbd "k") nil)
        (define-key tablist-minor-mode-map (kbd "/") nil)

        ;; Set some evil-like keybinds
        (evilify-maps (list docker-container-mode-map
                            docker-image-mode-map
                            docker-network-mode-map
                            docker-volume-mode-map))

        (evil-set-initial-state 'docker-container-mode 'emacs)
        (evil-set-initial-state 'docker-image-mode 'emacs)
        (evil-set-initial-state 'docker-network-mode 'emacs)
        (evil-set-initial-state 'docker-volume-mode 'emacs))

(use-package docker-tramp
  :straight t
  :defer t)

(use-package restclient
  :straight t)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(core/leader "ci" 'indent-buffer)

;; Doc view

(after! doc-view-mode
        (setq-default doc-view-resolution 200))

;; eshell

(core/leader
 "ss" 'eshell
 "sn" 'eshell-new)

(defface eshell-prompt-pwd '((t :inherit font-lock-constant-face))
  "TODO"
  :group 'eshell)

(defface eshell-prompt-git-branch '((t :inherit font-lock-builtin-face))
  "TODO"
  :group 'eshell)

(defun eshell--current-git-branch ()
  (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
                              if (string-match-p "^\*" match)
                              collect match))))
    (if (not (eq branch nil))
        (format " (%s)" (substring branch 2))
      "")))

(defun eshell-default-prompt ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (concat (if (bobp) "" "\n")
          (let ((pwd (eshell/pwd)))
            (propertize (if (equal pwd "~")
                            pwd
                          (abbreviate-file-name pwd))
                        'face 'eshell-prompt-pwd))
          (propertize (eshell--current-git-branch)
                      'face 'eshell-prompt-git-branch)
          (propertize " >" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))

(defun eshell-new ()
  (interactive)
  (eshell "new"))

(defun disable-completions-tramp ()
  (when (file-remote-p default-directory) (company-mode -1)))

(after! eshell
        (defalias 'eshell/d 'dired)
        (defalias 'eshell/ff 'find-file)
        (defalias 'eshell/async 'async-shell-buffer)

        (defun eshell/read-history ()
          (interactive)
          (insert (ido-completing-read "History: " (ring-elements eshell-history-ring))))

        (add-hook 'eshell-mode-hook #'disable-completions-tramp)

        (add-hook 'eshell-mode-hook
                  (lambda ()
                    ;; Needs to be ran inside the hook since eshell-mode-map is
                    ;; buffer local.
                    ;;
                    ;; See https://github.com/noctuid/general.el/issues/80
                    (local-set-key (kbd "C-c h") 'eshell/read-history)))

        (setq eshell-prompt-function #'eshell-default-prompt)
        (setq eshell-prompt-regexp "^.* > "))

;; shell

(after! shell
        (add-hook 'shell-mode-hook #'disable-completions-tramp))

;; gcloud

(use-package gcloud
  :straight (gcloud :type git :host github :repo "scsmithr/gcloud.el"))

(provide 'utils)
;;; utils.el ends here
