;;; seanmacs-flymake.el --- Flymake configuration and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Flymake configuration and helpers.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package flymake
  ;; built-in, ish. Eglot pulls in development versions.
  :demand t
  :config
  (setq flymake-start-on-save-buffer t
        flymake-no-changes-timeout 1
        flymake-fringe-indicator-position 'right-fringe
        flymake-mode-line-lighter "")

  (defun sm/flymake-sort-entries (entries)
    "Return a sorted list of ENTRIES where each entry is sorted by
severity descending, then line ascending."
    (sort entries
          #'(lambda (a b)
              (let* ((f (lambda (entry)
                          (cons (plist-get (car entry) :severity)
                                (plist-get (car entry) :line))))
                     (cmp-a (funcall f a))
                     (cmp-b (funcall f b)))
                (or (> (car cmp-a)
                       (car cmp-b))
                    (and (eq (car cmp-a)
                             (car cmp-b))
                         (< (cdr cmp-a)
                            (cdr cmp-b))))))))

  (advice-add 'flymake--diagnostics-buffer-entries :filter-return #'sm/flymake-sort-entries)

  (evil-define-key 'normal flymake-diagnostics-buffer-mode-map
    "p" #'sm/pop-to-some-window)
  (evil-define-key 'normal flymake-project-diagnostics-mode-map
    "p" #'sm/pop-to-some-window)

  :hook ((prog-mode . flymake-mode))
  :bind (:prefix "C-c f"
                 :prefix-map flymake-prefix-map
                 ("l" . consult-flymake)
                 ("b" . flymake-show-project-diagnostics)
                 ("n" . flymake-goto-next-error)
                 ("p" . flymake-goto-prev-error)))

(provide 'seanmacs-flymake)
;;; seanmacs-flymake.el ends here
