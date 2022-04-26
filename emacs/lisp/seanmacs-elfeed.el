;;; seanmacs-elfeed.el --- Elfeed configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Elfeed configuration.

;;; Code:

(use-package elfeed
  :straight t
  :defer t
  :config
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
          ("https://protesilaos.com/codelog.xml" blog updates)
          ("https://lemire.me/blog/feed/" blog dev)
          ("https://updates.orgmode.org/feed/changes" dev updates)
          ("https://emacsair.me/feed.xml" blog dev updates)
          ("https://www.micahlerner.com/feed.xml" blog dev)
          ("https://brooker.co.za/blog/rss.xml" blog dev)
          ("http://muratbuffalo.blogspot.com/feeds/posts/default" blog dev)
          ("http://charap.co/feed" blog dev)
          ("https://fasterthanli.me/index.xml" blog dev)
          ;; Papers
          ,(sm/arxiv-atom-feed "cs.DC" '(cs))
          ,(sm/arxiv-atom-feed "cs.DB" '(cs))
          ("https://db.cs.cmu.edu/files/rss/pvldb-atom.xml" cs)
          ))

  :bind(("C-c a e" . elfeed)
        :map elfeed-search-mode-map
        ("C-c C-u" . elfeed-update)
        :map elfeed-show-mode-map
        ("C-c C-o d" . sm/download-arxiv-paper)
        ("C-c C-o o" . sm/open-arxiv-paper)
        ("C-c C-o C-o" . sm/open-arxiv-paper)))

(defun sm/download-pdf (link &optional type open name)
  "Download and save a pdf from LINK.

TYPE may be one of paper, book, arxiv, or refile. When OPEN is
non-nil, open the downloaded pdf. Does not overwrite existing
files.

NAME is the name to save the pdf as. If nil, defaults to the
original file name. The '.pdf' extension will be appended if it's
missing from the name."
  (interactive (let ((link (read-string "Link: ")))
                 (list link
                       (completing-read "Type: " '(paper book arxiv refile))
                       t
                       (read-string "Name: " (file-name-nondirectory link)))))
  (let ((dir (cond ((string= type 'paper) "~/syncthing/papers/")
                   ((string= type 'book)  "~/syncthing/books/")
                   ((string= type 'arxiv) "~/syncthing/arxiv/downloads/")
                   (t                     "~/syncthing/refile/")))
        (name (or name (file-name-nondirectory link))))
    (mkdir dir t)
    (if (string-empty-p name)
        (user-error "Got empty file name from link: %s" link)
      (let ((path (concat dir (if (string-suffix-p ".pdf" name) name (concat name ".pdf")))))
        (if (file-exists-p path)
            (message "File already exists at %s" path)
          (url-copy-file link path))
        (when open (find-file path))))))

(defun sm/arxiv-link-to-pdf (link)
  "Modify LINK to point to the arxiv pdf if it's not already."
  (replace-regexp-in-string
   (regexp-quote "abs") "pdf" link nil 'literal))

(defun sm/download-arxiv-paper (link)
  "Download pdf for arxiv paper at LINK."
  (interactive (list (or (and (boundp 'elfeed-show-entry)
                              elfeed-show-entry
                              (elfeed-entry-link elfeed-show-entry))
                         (read-string "Arxiv link: "))))
  (sm/download-pdf (sm/arxiv-link-to-pdf link) 'arxiv t))

(defun sm/open-arxiv-paper (link)
  "Open pdf for arxiv paper at LINK."
  (interactive (list (or (and (boundp 'elfeed-show-entry)
                              elfeed-show-entry
                              (elfeed-entry-link elfeed-show-entry))
                         (read-string "Arxiv link: "))))
  (sm/download-pdf (sm/arxiv-link-to-pdf link) 'arxiv t))

(provide 'seanmacs-elfeed)
;;; seanmacs-flymake.el ends here
