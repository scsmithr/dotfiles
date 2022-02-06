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
  (setq whitespace-style '(face space-mark tab-mark lines-tail
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
        ibuffer-formats '((mark modified read-only
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
           ("search" (or
                      (mode . deadgrep-mode)
                      (mode . grep-mode)
                      (mode . ripgrep-mode)))
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
    (
     :name "Size"
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

(use-package project
  ;; Built-in, but using development version.
  :config

  (defun sm/project-root-maybe ()
    "Return the root of project if `default-directory' is in a project. Return nil otherwise."
    (when-let (project (project-current))
      (project-root)))

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
          (magit-status "Magit" ?g)
          (deadgrep "Search" ?s)))
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("e" . sm/project-eshell)
              ;; 's' is bound to `project-shell' by default, but I rarely use
              ;; shell, so rebind to some search functions.
              :prefix "s"
              :prefix-map project-search-prefix-map
              ("d" . sm/deadgrep-this-directory)
              ("D" . deadgrep)))

(use-package deadgrep
  :straight t
  :defer t
  :init
  (defun sm/deadgrep-this-directory ()
    "Call deadgrep using `default-directory' as the project root."
    (interactive)
    (let ((deadgrep-project-root-function (lambda () default-directory)))
      (call-interactively 'deadgrep)))
  :config
  (defun sm/deadgrep-show ()
    "Display result in other window, keeping the cursor in the deadgrep window."
    (interactive)
    (when (and (deadgrep--filename) (deadgrep--line-number))
      (sm/save-window-excursion
       (deadgrep-visit-result-other-window))))

  (evil-collection-define-key 'normal 'deadgrep-mode-map
    "gS" #'deadgrep-search-term
    "gD" #'deadgrep-directory
    "gE" #'deadgrep-edit-mode
    (kbd "SPC") #'sm/deadgrep-show))

(use-package ripgrep
  :straight t
  :config
  (evil-collection-define-key 'normal 'ripgrep-search-mode-map
    "q" 'quit-window))

(use-package flycheck
  :straight t
  :demand t
  :config
  (defun sm/flycheck-display-errors-function (errs)
    (let ((message-truncate-lines t))
      (flycheck-display-error-messages errs)))

  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-indication-mode 'right-fringe
        flycheck-display-errors-delay 1
        flycheck-display-errors-function #'sm/flycheck-display-errors-function
        ;; Increased from 400, eglot with rls will sometimes display a lot of
        ;; warnings, which I want to see.
        flycheck-checker-error-threshold 1000)
  (fset 'flycheck-command-map flycheck-command-map)

  (defalias 'sm/flycheck-error-list-goto-error-no-jump 'flycheck-error-list-goto-error)
  (evil-add-command-properties #'flycheck-error-list-goto-error :jump t)

  (defun sm/flycheck-error-list-show-error ()
    "Show location of error in the error list."
    (interactive)
    (sm/save-window-excursion (call-interactively 'sm/flycheck-error-list-goto-error-no-jump)))

  (evil-collection-define-key 'normal 'flycheck-error-list-mode-map
    (kbd "SPC") #'sm/flycheck-error-list-show-error)

  (global-flycheck-mode)
  :bind-keymap ("C-c f" . flycheck-command-map))

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

(use-package which-key
  :straight t
  :config
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

  :bind (("C-c e" . edit-indirect-region)))

(provide 'seanmacs-edit)
;;; seanmacs-edit.el ends here
