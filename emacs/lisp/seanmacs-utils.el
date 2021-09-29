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

  (defun sm/arxiv-pdf-name-from-link (link)
    "Get the relative pdf name from an arxiv LINK."
    (let ((paper-ref (car (last (split-string link "/")))))
      (if (string-suffix-p ".pdf" paper-ref)
          paper-ref ;; Link already points to pdf.
        (concat paper-ref ".pdf"))))

  (defun sm/expand-arxiv-paper-path (name)
    "Get the full path to where a paper should be."
    (concat (file-name-as-directory sm/arxiv-download-dir) name))

  (defun sm/download-arxiv-paper (link)
    "Download associated arxiv pdf from arxiv LINK.

If current buffer is an elfeed entry, use the link from the
entry. Otherwise prompt for an arxiv link. Returns the path to
the downloaded paper."
    (interactive (list (or (and (boundp 'elfeed-show-entry)
                                elfeed-show-entry
                                (elfeed-entry-link elfeed-show-entry))
                           (read-string "Arxiv link: "))))
    (let* ((pdf-link (replace-regexp-in-string
                      (regexp-quote "abs") "pdf" link nil 'literal)))
      (message (format "Downloading %s" pdf-link))
      (mkdir sm/arxiv-download-dir t)
      (let* ((name (sm/arxiv-pdf-name-from-link link))
             (full-path (sm/expand-arxiv-paper-path name)))
        (with-current-buffer (url-retrieve-synchronously pdf-link)
          (write-region nil nil full-path)
          full-path))))

  (defun sm/open-arxiv-paper (link)
    "Open pdf for arxiv paper at LINK. Download the paper if it doesn't exist."
    (interactive (list (or (and (boundp 'elfeed-show-entry)
                                elfeed-show-entry
                                (elfeed-entry-link elfeed-show-entry))
                           (read-string "Arxiv link: "))))
    (let* ((name (sm/arxiv-pdf-name-from-link link))
           (path (sm/expand-arxiv-paper-path name)))
      (when (not (file-exists-p path))
        (sm/download-arxiv-paper link))
      (find-file-other-window path)))

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
          ("http://www.math3ma.com/blog/rss.xml" math blog)
          ("https://protesilaos.com/codelog.xml" blog)
          ;; Papers
          ,(sm/arxiv-atom-feed "cs.LG" '(ai))
          ,(sm/arxiv-atom-feed "cs.AI" '(ai))
          ,(sm/arxiv-atom-feed "cs.DC" '(cs))
          ,(sm/arxiv-atom-feed "cs.DB" '(cs))
          ,(sm/arxiv-atom-feed "q-bio.NC" '(bio))
          ,(sm/arxiv-atom-feed "q-bio.QM" '(bio))
          ))

  :bind(("C-c a e" . elfeed)
        :map elfeed-search-mode-map
        ("C-c C-u" . elfeed-update)
        :map elfeed-show-mode-map
        ("C-c C-o d" . sm/download-arxiv-paper)
        ("C-c C-o o" . sm/open-arxiv-paper)
        ("C-c C-o C-o" . sm/open-arxiv-paper)))

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
  :bind (("C-x d" . dired-jump)
         :map dired-mode-map
         ("C-c C-n" . dired-next-subdir)
         ("C-c C-p" . dired-prev-subdir)))

(use-package diredfl
  :straight t
  :defer t)

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (defun sm/pdf-outline-show-link ()
    "Show link in pdf window, keeping focus in the outline."
    (interactive)
    (sm/save-window-excursion
     (call-interactively 'pdf-outline-follow-link)))

  (evil-collection-define-key 'normal 'pdf-outline-buffer-mode-map
    (kbd "<tab>") #'pdf-outline-follow-link
    (kbd "SPC") #'sm/pdf-outline-show-link))

(use-package ffap
  ;; built-in
  :demand t
  :init
  (defvar sm/ffap-disable-for-modes
    '(dired-mode) ;; I use C-x C-f exclusively to create files in dired.
    "List of major modes where `ffap' should not be used.")

  (defun sm/find-file ()
    "Dispatch to `find-file' and `ffap' as appropriate.

If file name under point contains a line number, jump to it after
opening the file."
    (interactive)
    (if (member major-mode sm/ffap-disable-for-modes)
        (call-interactively 'find-file)
      (let* ((str (ffap-string-at-point))
             (idx (string-match ":[0-9]+$" str))
             (file (if idx (substring str 0 idx) str))
             (line (when (and idx file) (substring str (+ 1 idx)))))
        (call-interactively 'find-file-at-point)
        (when line
          (goto-line (string-to-number line))
          (recenter nil)))))

  :bind (("C-x C-f" . sm/find-file)))

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
