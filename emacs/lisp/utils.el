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

        (defun add-evil-like-bindings (maps)
          (let ((map (car maps))
                (rest (cdr maps)))

            (define-key map "/" 'evil-search-forward)
            (define-key map ":" 'evil-ex)
            (define-key map "h" 'evil-backward-char)
            (define-key map "j" 'evil-next-visual-line)
            (define-key map "k" 'evil-previous-visual-line)
            (define-key map "l" 'evil-forward-char)
            (define-key map "n" 'evil-search-next)
            (define-key map "N" 'evil-search-previous)
            (define-key map "v" 'evil-visual-char)
            (define-key map "V" 'evil-visual-line)
            (define-key map "gg" 'evil-goto-first-line)
            (define-key map "G" 'evil-goto-line)
            (define-key map (kbd "C-f") 'evil-scroll-page-down)
            (define-key map (kbd "C-b") 'evil-scroll-page-up)
            (define-key map (kbd "C-e") 'evil-scroll-line-down)
            (define-key map (kbd "C-y") 'evil-scroll-line-up)
            (define-key map (kbd "C-d") 'evil-scroll-down)
            (define-key map (kbd "C-u") 'evil-scroll-up)
            (define-key map (kbd "C-z") 'evil-emacs-state)

            (evilify-window-switch map)

            (when rest (add-evil-like-bindings rest))))
        ;; Set some evil-like keybinds

        (add-evil-like-bindings (list docker-container-mode-map
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

(provide 'utils)
;;; utils.el ends here
