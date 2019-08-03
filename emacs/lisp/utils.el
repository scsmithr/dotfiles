;;; utils.el --- Utilities

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
  :after magit)

(use-package evil-magit
  :straight t)

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

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
