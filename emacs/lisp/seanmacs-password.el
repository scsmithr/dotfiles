;;; seanmacs-password.el --- Password helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for managing passwords.

;;; Code:

(require 'auth-source)

(defvar sm/password-auth-sources '("~/syncthing/passwords.gpg")
  "Where to store passwords.")

(defvar sm/password-use-symbols nil
  "Whether or not to include symbols in the generated password.")

(defvar sm/password-remove-after nil
  "Time in seconds before the password is removed from the kill ring.
If set to nil, some default duration will be used.")

(defun sm/generate-password ()
  "Generate a random password."
  (interactive)
  (if sm/password-use-symbols
      (shell-command-to-string "cat /dev/urandom | tr -dc 'a-zA-Z0-9-_!@#$%^&*()_+{}|:<>?=' | head -c 28")
    (shell-command-to-string "cat /dev/urandom | tr -dc 'a-zA-Z0-9' | head -c 28")))

(defun sm/password-insert-killring (pass)
  "Put PASS in kill ring, removing it after `sm/password-remove-after' or 15 seconds."
  (run-with-timer
   (or sm/password-remove-after 15) nil #'(lambda ()
                                            (message "Password removed from kill ring")
                                            (kill-new "" t)))
  (kill-new pass))

(defun sm/password-yank ()
  "Search for a password and put it in the kill ring, removing it after some time."
  (interactive)
  (let ((auth-sources sm/password-auth-sources)
        (auth-source-do-cache nil))
    (let ((options (mapcar #'(lambda (x)
                               (let ((host (plist-get x :host))
                                     (user (plist-get x :user)))
                                 (cons (format "%s/%s" host user)
                                       (list host user))))
                           (auth-source-search :max 1000))))
      (let* ((selected (completing-read "Credentials: " options nil t))
             (val (cdr (assoc selected options)))
             (host (car val))
             (user (cdr val)))
        (let ((src (auth-source-search :host host :user user)))
          (if src
              (let ((pass (funcall (plist-get (car src) :secret))))
                (sm/password-insert-killring pass))
            (message "Password not found")))))))

(defun sm/password-store (host user)
  "Generate and store a password for some HOST and USER.

The password will be added to the kill ring, and removed after
some time."
  (interactive (list
                (read-from-minibuffer "Host: ")
                (read-from-minibuffer "User: ")))
  (let ((password (sm/generate-password))
        (auth-sources sm/password-auth-sources)
        (auth-source-do-cache nil)
        (auth-source-save-behavior t))
    (if (auth-source-search :host host :user user :max 0)
        (message "Warning: credentials already exist for %s and %s" host user)
      (let ((src (auth-source-search :host host :user user :secret password :create t)))
        (funcall (plist-get (car src) :save-function))
        (sm/password-insert-killring password)))))

(provide 'seanmacs-password)
;;; seanmacs-libs.el ends here
