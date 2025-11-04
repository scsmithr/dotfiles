;;; seanmacs-edit.el --- Edit -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing configuration.

;;; Code:

(eval-when-compile
  (require 'use-package))

;; Highlight parenthesis
(show-paren-mode 1)

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width-start t)

;; Wrap column
(setq-default fill-column 80)

;; Scrolling
(setq-default scroll-conservatively 101
              hscroll-margin 3)

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(3)
      mouse-wheel-progressive-speed nil)

;; Configure file backups
(setq backup-directory-alist '((".*" . "~/.emacs.d/.backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/.backups" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; #Don't #create #lock #files
(setq create-lockfiles nil)

(setq sentence-end-double-space nil)

;; Auto insert closing parenthesis, braces, etc
(use-package elec-pair
  ;; built-in
  :config
  ;; Helps a lot with triple quoting.
  (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)
  (electric-pair-mode 1))

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

(defun ignore-errors-fn (orig-fn &rest args)
  "Ignore any errors resulting from calling ORIG-FN with ARGS."
  (ignore-errors (apply orig-fn args)))

;; Some modes that derive from prog-mode don't properly support hideshow
;; (specifically lean info mode). I don't really care if a mode doesn't support
;; hideshow.
(advice-add 'hs-grok-mode-type :around #'ignore-errors-fn)

(setq inhibit-compacting-font-caches t)

(setq-default require-final-newline t)

;; Always use "y or n"
(setq use-short-answers t)

;; Tab stuff
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Don't wrap lines in prog buffers.
(defun sm/truncate-lines ()
  "Truncate lines for the buffer."
  (toggle-truncate-lines 1))
(add-hook 'prog-mode-hook 'sm/truncate-lines)

;; Allow narrowing to region
(put 'narrow-to-region 'disabled nil)
;; Allow narrowing to page
(put 'narrow-to-page 'disabled nil)
;; Allow erasing entire buffer
(put 'erase-buffer 'disabled nil)

(defun sm/sudo-edit (file)
  "Open FILE in a new buffer with sudo.

Defaults to opening file for the current buffer."
  (interactive (list (or buffer-file-name
                         (read-file-name "File: "))))
  (find-file (concat "/sudo::" file)))

(use-package whitespace
  ;; built-in
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face space-mark tab-mark
                                trailing tabs spaces))
  (setq whitespace-display-mappings
        '((space-mark   ?\  [?·])
          (newline-mark ?\n [?$ ?\n])
          (tab-mark     ?\t [?→ ?\t])))

  (defun sm/reinitialize-whitespace-mode (&rest _)
    "If whitespace-mode is enabled, force it on again.

Useful for certain modes that fontify after the mode has been
loaded (e.g. sql-mode)."
    (when (bound-and-true-p whitespace-mode)
      (message "Reinitializing whitespace mode")
      (whitespace-mode t)))

  :hook ((prog-mode . whitespace-mode)))

(defvar sm/ibuffer-filter-group-order nil
  "Forced order for ibuffer's filter groups.")

(use-package ibuffer
  ;; built-in
  :init
  (defun sm/ibuffer-switch-to-saved-filter-groups ()
    "Switch to my custom filter group."
    (ibuffer-switch-to-saved-filter-groups "seanmacs"))

  (defun sm/ibuffer-jump-to-last-buffer ()
    "Jump to the most recently visited buffer in ibuffer."
    (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list)))))

  :config
  (setq ibuffer-read-only-char ?R
        ibuffer-eliding-string "…"
        ;; NOTE: Code directories contain "host" subdirs (e.g. github.com or
        ;; gitlab.org). I don't want those showing up in ibuffer so make sure we
        ;; match on those.
        ibuffer-directory-abbrev-alist `(("^/\\(Users\\|home\\)/sean/Code/[a-zA-Z.]+/"    . "[c] ")
                                         (,(concat "^" sm/sync-dir)                       . "[s] "))
        ibuffer-formats '((mark modified read-only
                                " " (name 28 28 :left :elide)
                                " " (mode 16 16 :left :elide)
                                " " (size 9 -1 :right)
                                " " filename-and-process))
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("seanmacs"
           ("Async" (name . "\*Async"))
           ("Compile" (mode . compilation-mode))
           ("Shell" (or
                     (mode . eshell-mode)
                     (mode . term-mode)
                     (mode . shell-mode)))
           ("Dired" (or (mode . dired-mode)
                        (mode . image-dired-thumbnail-mode)))
           ("Scratch" (name . "\*scratch"))
           ("Search" (or
                      (mode . rg-mode)
                      (mode . grep-mode)))
           ("Interactive" (or (mode . sql-interactive-mode)
                              (mode . geiser-repl-mode)))
           ("Special" (or (name . "\*")))
           ("Version control" (or
                               (name . "magit")
                               (mode . diff-mode)))
           ("Docs" (or
                    (mode . doc-view-mode)
                    (mode . pdf-view-mode))))))

  (setq sm/ibuffer-filter-group-order '("Default" "Docs" "Shell" "Interactive" "Special"))

  (defun sm/ibuffer-order-filter-groups (groups)
    "Sort GROUPS using `sm/ibuffer-filter-group-order' and then alphabetically."
    ;; Note that this sorts in reverse order because ibuffer reverses these
    ;; groups before printing.
    (sort groups
          (lambda (a b)
            (let ((apos (cl-position (car a) sm/ibuffer-filter-group-order :test 'equal))
                  (bpos (cl-position (car b) sm/ibuffer-filter-group-order :test 'equal)))
              (cond ((and apos bpos) (> apos bpos))
                    (apos nil)
                    (bpos t)
                    (t (string> (car a) (car b))))))))

  (advice-add 'ibuffer-generate-filter-groups :filter-return #'sm/ibuffer-order-filter-groups)

  ;; Allows me to use ':b' to quickly jump to the buffer I was at before opening
  ;; ibuffer.
  (advice-add 'ibuffer-visit-buffer :around #'sm/run-and-bury)

  (defun sm/ibuffer-backward-filter-group-skip-current ()
    "Jump to the previous filter group header."
    (interactive)
    (if (get-text-property (point) 'ibuffer-filter-group-name)
        (ibuffer-backward-filter-group 1)
      (ibuffer-backward-filter-group 3)))

  (evil-collection-define-key 'normal 'ibuffer-mode-map
    "gj" #'ibuffer-forward-filter-group
    "gk" #'sm/ibuffer-backward-filter-group-skip-current)

  :hook ((ibuffer-mode . sm/ibuffer-switch-to-saved-filter-groups)
         (ibuffer . sm/ibuffer-jump-to-last-buffer))
  :bind (:map buffer-prefix-map
              ("b" . ibuffer)))

(use-package dtrt-indent
  :straight t
  :init
  (defun sm/dtrt-readapt ()
    "Readapt dtrt indenting."
    (interactive)
    (dtrt-indent-undo)
    (dtrt-indent-adapt))

  :config
  (setq dtrt-indent-verbosity 0
        dtrt-indent-min-quality 80.0
        dtrt-indent-min-hard-tab-superiority 180.0)

  (dtrt-indent-global-mode 1))

(use-package project
  :config

  (defun sm/project-root-maybe ()
    "Return the root of project if `default-directory' is in a project. Return nil otherwise."
    (when-let (project (project-current))
      (project-root project)))

  (defun sm/project-eshell ()
    "Similiar to `project-eshell', opens eshell in the project root.

This use the default windowing rules for eshell when opening the
eshell buffer. In my case, this will open the eshell buffer in
the currently selected window instead of popping to some other
window."
    (interactive)
    (defvar eshell-buffer-name)
    (let* ((default-directory (project-root (project-current t)))
           (eshell-buffer-name (project-prefixed-buffer-name "eshell")))
      (eshell current-prefix-arg)))

  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (sm/project-eshell "Eshell" ?e)
          (project-find-dir "Find directory" ?d)
          (project-dired "Project root" ?o)
          (magit-status "Magit" ?m)))
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("e" . sm/project-eshell)))

(use-package rg
  :straight t
  :config
  (evil-collection-define-key 'normal 'rg-mode-map
    "gj" #'rg-next-file
    "gk" #'rg-prev-file
    (kbd "SPC") #'compilation-display-error)
  :bind (:map app-prefix-map
              ("r" . rg-menu)))

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

(use-package which-key
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode 1))

(use-package edit-indirect
  :straight t
  :commands edit-indirect-region
  :config
  (defun sm/edit-indirect-guess-mode (parent-buffer _beg _end)
    "Guess major mode by getting the mode from PARENT-BUFFER."
    (let ((mode (with-current-buffer parent-buffer
                  major-mode)))
      (if (eq mode 'fundamental-mode)
          (normal-mode)
        (funcall mode))))

  (setq edit-indirect-guess-mode-function #'sm/edit-indirect-guess-mode)

  :bind (:map buffer-prefix-map
              ("e" . edit-indirect-region)))

(use-package apheleia
  :straight t
  :config
  (setq apheleia-remote-algorithm 'remote)

  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format"
          "--style=file"
          "--assume-filename"
          (or
           (buffer-file-name)
           (cdr
            (assoc major-mode
                   '((c-mode . ".c")
                     (c++-mode . ".cpp")
                     (cuda-mode . ".cu")
                     (protobuf-mode . ".proto"))))
           ".c"))))

(use-package bookmark
  ;; built-in
  :config
  (setq bookmark-save-flag 1))

(provide 'seanmacs-edit)
;;; seanmacs-edit.el ends here
