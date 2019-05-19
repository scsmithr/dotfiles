;;; packages.el --- Packages for haskell

(defun haskell/init-haskell-mode ()
    (use-package haskell-mode
      :ensure t
      :defer t
      :config
      (setq haskell-stylish-on-save t)
      (setq haskell-mode-stylish-haskell-path "brittany")))

(provide 'haskell)
;;; packages.el ends here
