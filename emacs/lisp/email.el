;;; orgmode.el --- Email -*- lexical-binding: t; -*-

(use-package mu4e
  :commands (mu4e)
  :init
  ;; General configuration with mbsync.
  (setq user-full-name "Sean Smith"
        mu4e-maildir "~/.mail"
        mu4e-attachment-dir "~/.mail/.attachments"
        mu4e-get-mail-command "mbsync -a"
        mu4e-change-filenames-when-moving t))

;; Gmail specific things.
;; This is mostly taken from Doom emacs.
(after! mu4e
        (face-attr 'mu4e-highlight-face
                   :background (doom-color 'bg)
                   :foreground (doom-color 'blue))
        (face-attr 'mu4e-header-highlight-face
                   :inherit 'hl-line
                   :underline nil)

        (setq mu4e-sent-messages-behavior 'delete
              mu4e-index-cleanup nil
              mu4e-index-lazy-check t
              mu4e-compose-format-flowed t
              fill-flowed-display-column 72)

        (setq smtpmail-stream-type 'starttls
              message-send-mail-function #'smtpmail-send-it)

        (setq message-kill-buffer-on-exit t)

        (setq mu4e-maildir-shortcuts '(("/personal/INBOX" . ?p)
                                       ("/work/INBOX" . ?w)))

        (core/local 'mu4e-headers-mode-map
                    "r" 'mu4e-update-index
                    "u" 'mu4e-update-main-and-index)

        (defun gmail-fix-flags (mark msg)
          (pcase mark
            (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Drafts"))
            (`refile (mu4e-action-retag-message msg "-\\Inbox"))
            (`flag   (mu4e-action-retag-message msg "+\\Starred"))
            (`unflag (mu4e-action-retag-message msg "-\\Starred"))))
        (add-hook 'mu4e-mark-execute-pre-hook #'gmail-fix-flags))

(defun set-email-account (label letvars &optional default-p)
  (when-let (address (cdr (assq 'user-email-address letvars)))
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

(after! mu4e
        (set-email-account "personal"
                           '((mu4e-sent-folder . "/personal/[Gmail]/Sent Mail")
                             (mu4e-drafts-folder . "/personal/[Gmail]/Drafts")
                             (mu4e-trash-folder . "/personal/[Gmail]/Trash")
                             (mu4e-refile-folder . "/personal/[Gmail]/All Mail")
                             (smtpmail-smtp-user . "scsmithr@gmail.com")
                             (smtpmail-smtp-server . "smtp.gmail.com")
                             (smtpmail-smtp-service . 25)
                             (user-mail-address . "scsmithr@gmail.com"))
                           t)

        (set-email-account "work"
                           '((mu4e-sent-folder . "/work/[Gmail]/Sent Mail")
                             (mu4e-drafts-folder . "/work/[Gmail]/Drafts")
                             (mu4e-trash-folder . "/work/[Gmail]/Trash")
                             (mu4e-refile-folder . "/work/[Gmail]/All Mail")
                             (smtpmail-smtp-user . "sean@coder.com")
                             (smtpmail-smtp-server . "smtp.gmail.com")
                             (smtpmail-smtp-service . 25)
                             (user-mail-address . "sean@coder.com"))
                           t))

(provide 'email)
;;; email.el ends here
