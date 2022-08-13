;;; seanmacs-email.el --- Email -*- lexical-binding: t; -*-

;;; Commentary:
;; Email configuration.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package mu4e
  ;; Provided by mu system package.
  :commands (mu4e)
  :config
  (setq user-full-name "Sean Smith"
        mu4e-attachment-dir "~/.mail/.attachments")

  ;; mu4e defaults to using the gnus message viewer. Block all images.
  (setq gnus-blocked-images ".")

  ;; General configuration with mbsync.
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-change-filenames-when-moving t)

  ;; Gmail specific things.
  (setq mu4e-sent-messages-behavior 'delete)

  ;; Seems to properly quote in gmail.
  (setq message-citation-line-format "\nOn %a, %B %d, %Y at %I:%M %p %f wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line)

  (setq mu4e-completing-read-function 'completing-read
        mu4e-compose-dont-reply-to-self t)

  (setq mu4e-use-fancy-chars nil
        mu4e-headers-thread-blank-prefix '("  " . " ")
        mu4e-headers-thread-child-prefix '(">>" . " ")
        mu4e-headers-thread-last-child-prefix '("->" . " ")
        mu4e-headers-thread-connection-prefix '("| " . " ")
        mu4e-headers-thread-orphan-prefix '("> " . " ")
        mu4e-headers-thread-single-orphan-prefix '("> " . " ")
        mu4e-headers-thread-duplicate-prefix '("=" . " "))

  (setq smtpmail-stream-type 'starttls
        message-send-mail-function #'smtpmail-send-it
        message-confirm-send t)

  (setq message-kill-buffer-on-exit t)

  (setq mu4e-maildir-shortcuts '(("/personal/INBOX" . ?p)
                                 ("/glare/INBOX"    . ?g)))

  (setq mu4e-bookmarks
        '((:query "flag:unread and date:14d..now"
           :name "Unread messages"
           :key ?u)
          (:query "date:today..now"
           :name "Today's messages"
           :key ?t)
          (:query "flag:flagged"
           :name "Flagged messages"
           :key ?f)))

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

  (defun sm/gmail-fix-flags (mark msg)
    (pcase mark
      (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Drafts"))
      (`refile (mu4e-action-retag-message msg "-\\Inbox"))
      (`flag   (mu4e-action-retag-message msg "+\\Starred"))
      (`unflag (mu4e-action-retag-message msg "-\\Starred"))))
  (add-hook 'mu4e-mark-execute-pre-hook #'sm/gmail-fix-flags)

  (defun sm/set-email-account (label letvars)
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
      context))

  (sm/set-email-account "personal"
                        '((mu4e-sent-folder . "/personal/[Gmail]/Sent Mail")
                          (mu4e-drafts-folder . "/personal/[Gmail]/Drafts")
                          (mu4e-trash-folder . "/personal/[Gmail]/Trash")
                          (mu4e-refile-folder . "/personal/[Gmail]/All Mail")
                          (smtpmail-smtp-user . "scsmithr@gmail.com")
                          (smtpmail-smtp-server . "smtp.gmail.com")
                          (smtpmail-smtp-service . 587)
                          (user-mail-address . "scsmithr@gmail.com")))

  (sm/set-email-account "glare"
                        '((mu4e-sent-folder . "/glare/[Gmail]/Sent Mail")
                          (mu4e-drafts-folder . "/glare/[Gmail]/Drafts")
                          (mu4e-trash-folder . "/glare/[Gmail]/Trash")
                          (mu4e-refile-folder . "/glare/[Gmail]/All Mail")
                          (smtpmail-smtp-user . "sean@glaredb.com")
                          (smtpmail-smtp-server . "smtp.gmail.com")
                          (smtpmail-smtp-service . 587)
                          (user-mail-address . "sean@glaredb.com")))

  :hook ((mu4e-compose-mode . flyspell-mode)
         (mu4e-compose-mode . turn-off-auto-fill)
         (mu4e-compose-mode . turn-on-visual-line-mode))
  :bind (("C-c a m" . mu4e)))

(use-package org-mu4e
  ;; Provided by mu system package.
  :after mu4e)

(use-package mm-decode
  ;; built-in
  :config
  (setq mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")))

(provide 'seanmacs-email)
;;; seanmacs-email.el ends here
