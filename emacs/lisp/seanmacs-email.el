;;; seanmacs-email.el --- Email -*- lexical-binding: t; -*-

;;; Commentary:
;; Email configuration.

;;; Code:

(use-package mu4e
  ;; Provided by mu system package.
  :commands (mu4e)
  :config
  ;; General configuration with mbsync.
  (setq user-full-name "Sean Smith"
        mu4e-maildir "~/.mail"
        mu4e-attachment-dir "~/.mail/.attachments"
        mu4e-change-filenames-when-moving t)

  (setq mu4e-get-mail-command "true" ;; Mail already retrieved by systemd unit.
        mu4e-update-interval (* 60 5)) ;; Reindex every 5 minutes.

  ;; Prefer plaintext.
  (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

  ;; Gmail specific things.
  ;; This is mostly taken from Doom emacs.
  (setq mu4e-sent-messages-behavior 'delete
        mu4e-index-cleanup nil
        mu4e-index-lazy-check t)

  ;; Seems to properly quote in gmail.
  (setq message-citation-line-format "\nOn %e %B %Y %R, %f wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line)

  (setq mu4e-completing-read-function 'completing-read
        mu4e-view-show-addresses t
        mu4e-compose-dont-reply-to-self t)

  (setq mu4e-use-fancy-chars nil
        mu4e-headers-thread-blank-prefix '("   " . " ")
        mu4e-headers-thread-child-prefix '("|->" . " ")
        mu4e-headers-thread-last-child-prefix '("-->" . " ")
        mu4e-headers-thread-connection-prefix '("|  " . " ")
        mu4e-headers-thread-orphan-prefix '("+->" . " ")
        mu4e-headers-thread-single-orphan-prefix '(" ->" . " ")
        mu4e-headers-thread-duplicate-prefix '("=" . " "))

  (setq smtpmail-stream-type 'starttls
        message-send-mail-function #'smtpmail-send-it)

  (setq message-kill-buffer-on-exit t)

  (setq mu4e-maildir-shortcuts '(("/personal/INBOX" . ?p)
                                 ("/work/INBOX"     . ?w)))

  (setq mu4e-bookmarks
        '(("flag:unread"
           "Unread messages"
           ?u)
          ("date:today..now"
           "Today's messages"
           ?t)
          ("subject:cdr or from:coder.com or from:clubhouse.io"
           "Coder messages"
           ?c)))

  ;; Remove mailing list, add account.
  (setq mu4e-headers-fields
        '((:account    . 10)
          (:human-date . 12)
          (:flags      . 4)
          (:from       . 22)
          (:subject    . nil)))

  ;; Add a column to display what email account the email belongs to.
  ;; Taken from Doom Emacs.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

  (defun gmail-fix-flags (mark msg)
    (pcase mark
      (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Drafts"))
      (`refile (mu4e-action-retag-message msg "-\\Inbox"))
      (`flag   (mu4e-action-retag-message msg "+\\Starred"))
      (`unflag (mu4e-action-retag-message msg "-\\Starred"))))
  (add-hook 'mu4e-mark-execute-pre-hook #'gmail-fix-flags)

  (defun set-email-account (label letvars &optional default-p)
    (when-let (address (cdr (assq 'user-mail-address letvars)))
      (add-to-list 'mu4e-user-mail-address-list address))
    (setq mu4e-contexts
          (cl-loop for context in mu4e-contexts
                   unless (string= (mu4e-context-name context) label)
                   collect context))
    (let ((context (make-mu4e-context
                    :name label
                    :enter-func (lambda () (mu4e-message "Switched to %s" label))
                    :leave-func #'mu4e-clear-caches
                    :match-func
                    (lambda (msg)
                      (when msg
                        (string-prefix-p (format "/%s" label)
                                         (mu4e-message-field msg :maildir))))
                    :vars letvars)))
      (push context mu4e-contexts)
      (when default-p
        (setq-default mu4e-context-current context))
      context))

  (set-email-account "personal"
                     '((mu4e-sent-folder . "/personal/[Gmail]/Sent Mail")
                       (mu4e-drafts-folder . "/personal/[Gmail]/Drafts")
                       (mu4e-trash-folder . "/personal/[Gmail]/Trash")
                       (mu4e-refile-folder . "/personal/[Gmail]/All Mail")
                       (smtpmail-smtp-user . "scsmithr@gmail.com")
                       (smtpmail-smtp-server . "smtp.gmail.com")
                       (smtpmail-smtp-service . 587)
                       (user-mail-address . "scsmithr@gmail.com"))
                     t)

  (set-email-account "work"
                     '((mu4e-sent-folder . "/work/[Gmail]/Sent Mail")
                       (mu4e-drafts-folder . "/work/[Gmail]/Drafts")
                       (mu4e-trash-folder . "/work/[Gmail]/Trash")
                       (mu4e-refile-folder . "/work/[Gmail]/All Mail")
                       (smtpmail-smtp-user . "sean@coder.com")
                       (smtpmail-smtp-server . "smtp.gmail.com")
                       (smtpmail-smtp-service . 587)
                       (user-mail-address . "sean@coder.com"))
                     t)

  (defun sm/mu4e-fetch-and-index ()
    "Fetch emails and reindex."
    (interactive)
    (let ((buf "*mu4e fetch*")
          (cmd "mbsync -Va"))
      (async-shell-command cmd buf)
      (mu4e-update-index)))

  :hook ((mu4e-compose-mode . turn-off-auto-fill)
         (mu4e-compose-mode . flyspell-mode))
  :bind (:map mu4e-headers-mode-map
              ("C-c r" . mu4e-update-index)
              ("C-c u" . sm/mu4e-fetch-and-index)))

(use-package org-mu4e
  ;; Provided by mu system package.
  :after mu4e)

(provide 'seanmacs-email)
;;; seanmacs-email.el ends here
