;;; seanmacs-edit.el --- Edit -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing configuration.

;;; Code:

;; Highlight parenthesis
(show-paren-mode 1)

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width-start t)

;; Autowrap text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Highlight current line
(global-hl-line-mode +1)

;; Wrap column
(setq-default fill-column 80)

;; Scrolling
(setq-default scroll-step 1
              scroll-margin 0
              scroll-conservatively 101
              hscroll-margin 0
              hscroll-step 1)

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

;; Auto insert closing parenthesis, braces, etc
(use-package elec-pair
  ;; built-in
  :init
  ;; Helps a lot with triple quoting.
  (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)
  :config
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
(defalias 'yes-or-no-p 'y-or-n-p)

;; Tab stuff
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Don't wrap lines
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

;; Allow narrowing to region
(put 'narrow-to-region 'disabled nil)
;; Allow narrowing to page
(put 'narrow-to-page 'disabled nil)

;; Allow erasing entire buffer
(put 'erase-buffer 'disabled nil)

(defun sm/sudo-edit ()
  "Open current file in a new buffer with sudo."
  (interactive)
  (let ((file (or buffer-file-name
                  (read-file-name "File: "))))
    (find-file (concat "/sudo::" file))))

(use-package whitespace
  ;; built-in
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(
                           face
                           space-mark
                           tab-mark lines-tail
                           trailing
                           tabs
                           spaces))
  :hook ((prog-mode . whitespace-mode)))

(defvar sm/ibuffer-filter-group-order nil
  "Forced order for ibuffer's filter groups.")

(use-package ibuffer
  ;; built-in
  :init
  (defun sm/ibuffer-switch-to-saved-filter-groups ()
    (ibuffer-switch-to-saved-filter-groups "seanmacs"))

  (defun sm/ibuffer-jump-to-last-buffer ()
    (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list)))))
  :config
  (setq ibuffer-read-only-char ?R)
  (setq ibuffer-formats
        '((mark modified read-only
                " " (name 24 24 :left :elide)
                " " (size-h 9 9 :right)
                " " (mode 16 16 :left :elide)
                " " filename-and-process))
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("seanmacs"
           ("async" (name . "\*Async"))
           ("compile" (mode . compilation-mode))
           ("shell" (or
                     (mode . eshell-mode)
                     (mode . term-mode)
                     (mode . shell-mode)))
           ("dired" (or (mode . dired-mode)
                        (mode . image-dired-thumbnail-mode)))
           ("email" (name . "\*mu4e"))
           ("scratch" (name . "\*scratch"))
           ("special" (or (name . "\*")
                          (mode . geiser-repl-mode)))
           ("version control" (or
                               (name . "magit")
                               (mode . diff-mode)
                               (mode . github-review-mode))))))

  (setq sm/ibuffer-filter-group-order '("Default" "shell" "special"))

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

  (define-ibuffer-column size-h
    (:name "Size"
           :inline t
           :summarizer
           ;; OPTIMIZE: Would be better to get the original values and sum those.
           (lambda (strings)
             (file-size-human-readable
              (seq-reduce
               (lambda (total value)
                 (let* ((suffixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y"))
                        (suffix (car (member (substring value -1) suffixes)))
                        (power 1000.0)
                        (bytes (string-to-number value)))
                   (while (and suffix (car suffixes) (not (string= (car suffixes) suffix)))
                     (setq bytes (* bytes power)
                           suffixes (cdr suffixes)))
                   (+ total bytes)))
               strings 0)
              "si")))
    (file-size-human-readable (buffer-size) "si"))

  ;; Allows me to use ':b' to quickly jump to the buffer I was at before opening
  ;; ibuffer.
  (advice-add 'ibuffer-visit-buffer :around #'sm/run-and-bury)

  :hook ((ibuffer-mode . sm/ibuffer-switch-to-saved-filter-groups)
         (ibuffer . sm/ibuffer-jump-to-last-buffer))
  :bind (("C-c b b" . ibuffer)))

(use-package dtrt-indent
  :straight t
  :init
  (defun sm/dtrt-readapt ()
    "Readapt dtrt indenting."
    (interactive)
    (dtrt-indent-undo)
    (dtrt-indent-adapt))
  :config
  (setq dtrt-indent-min-quality 65.0
        dtrt-indent-min-hard-tab-superiority 180.0)
  (dtrt-indent-global-mode 1))

(use-package projectile
  :straight t
  :config
  (setq projectile-completion-system 'default
        projectile-require-project-root nil
        projectile-sort-order 'recently-active
        projectile-switch-project-action #'projectile-dired
        ;; When you have a mono repo with a ton of submodules, caching helps
        ;; speed things up...
        projectile-enable-caching t)

  ;; Register project for purescript.
  (projectile-register-project-type 'purs '("spago.dhall")
                                    :compile "spago build"
                                    :test "spago test"
                                    :run "spago run")

  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("s d" . deadgrep)))

(use-package deadgrep
  :straight t
  :after projectile
  :config
  (defun sm/deadgrep-change-search ()
    "Change deadgrep search term."
    (interactive)
    (deadgrep--search-term nil))

  (defun sm/deadgrep-change-dir ()
    "Change deadgrep current search directory."
    (interactive)
    (deadgrep--directory nil))

  (evil-collection-define-key 'normal 'deadgrep-mode-map
    "s" #'sm/deadgrep-change-search
    "d" #'sm/deadgrep-change-dir
    "e" #'deadgrep-edit-mode))

(use-package ripgrep
  :straight t
  :after projectile
  :config
  (evil-collection-define-key 'normal 'ripgrep-search-mode-map
    "q" 'quit-window))

(use-package flycheck
  :straight t
  :demand t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-indication-mode 'right-fringe
        flycheck-display-errors-delay 0.2)
  (fset 'flycheck-command-map flycheck-command-map)
  (global-flycheck-mode)
  :bind-keymap ("C-c f" . flycheck-command-map))

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(provide 'seanmacs-edit)
;;; seanmacs-edit.el ends here
