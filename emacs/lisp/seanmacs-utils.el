;;; seanmacs-utils.el --- Utils -*- lexical-binding: t; -*-

;;; Commentary:
;; Other utilities.

;;; Code:

(use-package epg
  ;; built-in
  :init
  (setq auth-sources '("~/.authinfo.gpg" "~/.netrc.gpg")
        epg-pinentry-mode 'loopback))

(use-package tabulated-list
  ;; built-in
  :init
  (setq tabulated-list-gui-sort-indicator-asc ?↓
        tabulated-list-gui-sort-indicator-desc ?↑))

;; Quickly generate lang formatters.
(use-package reformatter
  :straight t)

(use-package shr
  ;; builtin
  :config
  (setq shr-width 80))

(use-package docker
  :straight t
  :defer t
  :bind (("C-c a d" . docker)))

(use-package docker-tramp
  :straight t
  :defer t)

(use-package elfeed
  :straight t
  :defer t
  :init

  (defvar sm/arxiv-number-results 100
    "Number of results to fetch from Arxiv.")

  (defun sm/arxiv-atom-feed (cat &optional tags)
    "Create an elfeed arxiv feed for CAT. `arxiv' will be added to TAGS."
    (let ((url (format "http://export.arxiv.org/api/query?search_query=cat:%s&start=0&max_results=%d&sortBy=submittedDate&sortOrder=descending"
                       cat
                       sm/arxiv-number-results)))
      (append (list url 'arxiv) tags)))

  (defun sm/extract-cat-from-atom-arxiv-feed (title)
    "Return arxiv category from TITLE or nil if title isn't an atom arxiv query."
    (when (s-contains-p "arxiv" (s-downcase title))
      (nth 1
           (split-string
            (nth 0
                 (split-string
                  title
                  "&")) "cat:"))))

  (defun sm/elfeed-concat-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (mapconcat
     (lambda (author) (plist-get author :name))
     authors-list ", "))

  (defvar sm/elfeed-search-author-min-width 16
    "Min width to display author column.")

  (defvar sm/elfeed-search-author-max-width 70
    "Max width to display author column.")

  (setq elfeed-search-title-max-width 100)

  (defun sm/elfeed-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title)
                      (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (let ((title (or (elfeed-meta feed :title)
                               (elfeed-feed-title feed))))
                ;; Display category instead of entire query if this an arxiv
                ;; feed. Display original title otherwise.
                (or (sm/extract-cat-from-atom-arxiv-feed title)
                    title))))
           (entry-authors (sm/elfeed-concat-authors
                           (elfeed-meta entry :authors)))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face
                                              'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width) 10
                           elfeed-search-trailing-width))
           (clamped-title-width (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width))
           (title-column (elfeed-format-column
                          title clamped-title-width
                          :left))
           (authors-width (- (window-width) 60 clamped-title-width))
           (authors-column (elfeed-format-column
                            entry-authors (elfeed-clamp
                                           sm/elfeed-search-author-min-width
                                           authors-width
                                           sm/elfeed-search-author-max-width)
                            :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column
                          'face title-faces 'kbd-help title) " ")
      (insert (propertize authors-column
                          'face 'elfeed-search-date-face
                          'kbd-help entry-authors) " ")
      (when feed-title
        (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert "(" tags-str ")"))))

  (defvar sm/arxiv-download-dir "~/syncthing/arxiv/downloads/"
    "Download directory for papers from arxiv.")

  (defun sm/download-arxiv-paper (link)
    "Download associated arxiv pdf from arxiv LINK.

If current buffer is an elfeed entry, use the link from the
entry. Otherwise prompt for an arxiv link."
    (interactive (list (or (and elfeed-show-entry
                                (elfeed-entry-link elfeed-show-entry))
                           (read-string "Arxiv link: "))))
    (let* ((pdf-link (replace-regexp-in-string
                      (regexp-quote "abs") "pdf" link nil 'literal)))
      (message (format "Downloading %s" pdf-link))
      (mkdir sm/arxiv-download-dir t)
      (let* ((name (concat (car (last (split-string link "/")))
                           ".pdf"))
             (cmd (format "cd %s && curl --silent -L -o %s %s"
                          sm/arxiv-download-dir name pdf-link)))
        (shell-command cmd))))

  :config
  (setq elfeed-db-directory "~/syncthing/elfeed/db"
        elfeed-search-print-entry-function #'sm/elfeed-print-entry)
  (setq elfeed-feeds
        `(
          ;; Blogs
          ("https://lexi-lambda.github.io/feeds/all.rss.xml" dev blog)
          ("http://dtrace.org/blogs/feed/" dev blog)
          ("http://www.stochasticlifestyle.com/feed/" dev sci blog)
          ("https://www.stephendiehl.com/feed.rss" dev blog)
          ("https://blog.cryptographyengineering.com/feed/" crypto blog)
          ("https://marc-b-reynolds.github.io/feed.xml" dev blog math)
          ("https://doisinkidney.com/rss.xml" math dev blog)
          ("https://buttondown.email/hillelwayne/rss" math dev newsletter)
          ("http://www.philipzucker.com/feed.xml" dev blog)
          ("https://terrytao.wordpress.com/feed" math blog)
          ;; Papers
          ,(sm/arxiv-atom-feed "cs.LG" '(ai))
          ,(sm/arxiv-atom-feed "cs.AI" '(ai))
          ,(sm/arxiv-atom-feed "cs.DC")
          ,(sm/arxiv-atom-feed "q-bio.NC")
          ,(sm/arxiv-atom-feed "q-bio.QM")
          ))

  :bind(("C-c a e" . elfeed)
        :map elfeed-search-mode-map
        ("C-c C-u" . elfeed-update)
        :map elfeed-show-mode-map
        ("C-c d a" . sm/download-arxiv-paper)))

(use-package restclient
  :straight t)

(use-package dired
  ;; built-in
  :config
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

  (defun sm/dired-maybe-insert-subdir (dirnames)
    "Insert subdirs for all DIRNAMES.

When called interactively, DIRNAMES will be all currently marked
files in the current dired buffer. If no files are marked, the
file at point will be used."
    (interactive (list (dired-get-marked-files)))
    (dolist (dirname dirnames)
      (dired-maybe-insert-subdir dirname)))

  (evil-collection-define-key 'normal 'dired-mode-map
    "K" #'dired-kill-subdir
    "I" #'sm/dired-maybe-insert-subdir
    "]]" #'dired-next-subdir
    "[[" #'dired-prev-subdir
    (kbd "TAB") 'dired-hide-subdir)

  (defun sm/dired-with-current-directory (orig-fn &rest args)
    "Set `default-directory' to dired's current dir if in dired mode."
    (if (eq major-mode 'dired-mode)
        (let ((default-directory (dired-current-directory)))
          (apply orig-fn args))
      (apply orig-fn args)))

  ;; Make find-file subdir aware.
  (advice-add 'find-file-read-args :around 'sm/dired-with-current-directory)

  :hook ((dired-mode . diredfl-mode))
  :bind(("C-x d" . dired-jump)
        :map dired-mode-map
        ("C-c C-n" . dired-next-subdir)
        ("C-c C-p" . dired-prev-subdir)))

(use-package diredfl
  :straight t
  :defer t)

(use-package dired-narrow
  :straight t)

(use-package dired-open
  :straight t
  :config
  (when (eq system-type 'gnu/linux)
    (setq dired-open-extensions '(("pdf" . "xdg-open")))))

(use-package help
  ;; built-in
  :config)

(use-package tramp-sh
  ;; built-in
  :config
  (setq tramp-remote-path '(tramp-own-remote-path "/usr/bin" "/bin")
        tramp-ssh-controlmaster-options (concat "-o ControlMaster=auto "
                                                "-o ControlPath='tramp.%%C' "
                                                "-o ControlPersist=yes")))

(use-package tramp
  ;; built-in
  :config
  (setq tramp-verbose 3))

(use-package gcloud
  :straight (gcloud :type git :host github :repo "scsmithr/gcloud.el"))

(use-package kube
  :straight (kube :type git :host github :repo "scsmithr/kube.el"))

(use-package coder
  :straight (coder :type git :host github :repo "cdr/coder.el"))

(provide 'seanmacs-utils)
;;; seanmacs-utils.el ends here
