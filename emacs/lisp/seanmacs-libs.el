;;; seanmacs-libs.el --- Functions/libraries -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions and libraries that should be loaded early on, and can be used
;; elsewhere within my configuration.

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cl-lib)

(defvar sm/dotfiles-dir (expand-file-name "~/dotfiles/")
  "Root directory containing my dotfiles.")

(defvar sm/sync-dir (expand-file-name "~/Library/Mobile Documents/com~apple~CloudDocs/")
  "Root directory for file syncing.")

(defun sm/dired-sync-dir ()
  "Open Dired inside the configured sync dir."
  (interactive)
  (dired sm/sync-dir))

(defun sm/md-note (name)
  "Open a new markdown note using NAME inside `sm/sync-dir'."
  (interactive (list (read-string "Note name: ")))
  (let* ((name (replace-regexp-in-string " " "-" name))
         (date-str (format-time-string "%m-%d-%y"))
         (file-name (format "%s/notes/md/%s-%s.md" sm/sync-dir date-str name)))
    (find-file file-name)))

;; Useful string utilities, e.g. 's-contains-p'.
(use-package s :straight t)

(use-package dash :straight t)

(defun sm/run-and-bury (fn &rest args)
  "Run FN with ARGS then bury the buffer."
  (let ((buf (buffer-name)))
    (apply fn args)
    (bury-buffer buf)))

(defun sm/unfill-paragraph (beg end)
  "Turn a paragraph into a single line of text."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph nil))))

(define-key global-map (kbd "M-Q") #'sm/unfill-paragraph)

(defun sm/indent-region-or-buffer (start end)
  "Indent region from START to END, or the entire buffer if no region is selected."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (indent-region start end))

(bind-keys :map buffer-prefix-map ("i" . sm/indent-region-or-buffer))

(defun sm/ansi-colorize (beg end)
  "Colorize region according to ANSI control sequences from BEG to END.
If no region selected, colorize the entire buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))

;; Tramp and filepath helpers

(defun sm/path-localname (path)
  "Return the localname of PATH if it's a remote file, or just PATH otherwise."
  (if (file-remote-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))

;; Testing helpers

(defvar sm/test-root "~/dotfiles/emacs/test/"
  "Directory containing test files.")

(defun sm/load-tests ()
  "Load test files from `sm/test-root'."
  (interactive)
  (mapcar #'load-file (directory-files sm/test-root t ".el$")))

(defun sm/run-all-tests ()
  "Load and run all tests."
  (interactive)
  (sm/load-tests)
  (ert t))

;; Kill ring helpers

(defun sm/kill-ring-alist (kill-ring)
  "Create an alist of (CLEANED . ORIGINAL) for a KILL-RING."
  (let ((alist))
    (dolist (item kill-ring)
      (push (cons (sm/clean-kill-item item) item) alist))
    (reverse alist)))

(defun sm/clean-kill-item (item)
  "Clean ITEM for use in a single line context."
  (set-text-properties 0 (length item) nil item)
  (let ((replace-fn (lambda (regexp rep string)
                      (replace-regexp-in-string regexp rep string nil t))))
    (let ((cleaned (thread-last item
                     (funcall replace-fn "\n" "\\n")
                     (funcall replace-fn "^\t*" "")
                     (funcall replace-fn "\t" "\\t")
                     (funcall replace-fn "^[ ]*" "")))
          (truncate-width 80))
      (if (> (length cleaned) truncate-width)
          (truncate-string-to-width cleaned truncate-width 0 0 "...")
        cleaned))))

(defun sm/insert-from-kill-ring-at-point ()
  "Select an item from the kill ring and insert it at point."
  (interactive)
  (let ((selectrum-should-sort nil) ;; Keep order of kill-ring
        (alist (sm/kill-ring-alist kill-ring)))
    (let ((selected (completing-read "Item: " alist nil t)))
      (when-let ((entry (assoc selected alist)))
        (insert (cdr entry))))))

;; Git stuff

(defvar sm/code-dir "~/Code")

(defconst sm/https-git-url-regexp "^https://\\([a-z.]+\\)/\\(.+\\)$")
(defconst sm/ssh-git-url-regexp "^git@\\([a-z.]+\\):\\(.+\\)$")

(defun sm/format-git-ssh-url (host path)
  (format "git@%s:/%s" host path))

(defun sm/parse-clone-url (url)
  (when (cond
         ((string-match sm/https-git-url-regexp url) t)
         ((string-match sm/ssh-git-url-regexp url) t)
         (t nil)))
  (let ((host (match-string 1 url))
        (path (file-name-sans-extension
               (match-string 2 url))))
    (list 'host host
          'path path
          'clone-url (sm/format-git-ssh-url host path))))

(defun sm/git-clone (url)
  "Clone URL into a subdirectory of `sm/code-dir'.

Opens the directory after the repo is finished cloning. Opens the
directory immediately if it already exists."
  (interactive (list (read-string "Git url: ")))
  (let ((props (sm/parse-clone-url url)))
    (when (not props)
      (user-error "Could not parse git url: %s" url))
    (let* ((host (plist-get props 'host))
           (path (plist-get props 'path))
           (clone-dir (string-join (list sm/code-dir host path) "/")))
      (if (file-exists-p clone-dir)
          (progn
            (message "%s already exists" clone-dir)
            (dired clone-dir))
        (let ((out-buf (generate-new-buffer (format "*Git clone [%s]*" path)))
              (clone-url (plist-get props 'clone-url)))
          (async-shell-command (format "git clone %s %s" clone-url clone-dir)
                               out-buf)
          (let ((proc (get-buffer-process out-buf)))
            (if (process-live-p proc)
                (set-process-sentinel proc (sm/create-git-clone-sentinel clone-dir))
              (message "No process"))))))))

(defun sm/create-git-clone-sentinel (clone-dir)
  "Return a sentinel that will open CLONE-DIR on completion."
  (lambda (proc change)
    (when (equal "finished" (string-trim change))
      (let ((buf (process-buffer proc)))
        (with-current-buffer buf
          (with-selected-window (get-buffer-window buf)
            (dired clone-dir)))))))

;; Text alignment

(defun sm/align-whitespace (start end)
  "Align columns by whitespace."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 1 t))

;; Misc

(defun sm/scratch ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (and initial-major-mode (funcall initial-major-mode))
    (insert initial-scratch-message)))

(defun sm/warn-fn-not-bound (fn-symbol)
  "Warn if FN-SYMBOL is void."
  (when (not (fboundp fn-symbol))
    (message "WARN: %s is void!" fn-symbol)))

(provide 'seanmacs-libs)
;;; seanmacs-libs.el ends here

