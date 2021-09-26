;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;;; Commentary:
;; Early Emacs configuration.

;;; Code:

;; Hide some things.
;; Do this first so that I never see the menu bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-x-resources t
      inhibit-startup-message t
      use-dialog-box nil)

;; Using straight to manage packages, don't need this.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
