;;; shco.el --- Shadow Completions -*- lexical-binding: t; -*-

;;; Commentary:
;; A replacement for Company's preview frontend.

;;; Code:

(defgroup shco nil
  "Shadow completions."
  :group 'shco)

(defcustom shco-preview-delay 0.2
  "Delay before showing completion previews."
  :type 'float
  :group 'shco)

(defcustom shco-min-prefix-length 1
  "Try completing once the prefix string his at least this length."
  :type 'integer
  :group 'shco)

(defvar-local shco--preview-timer nil)

(defvar-local shco--preview-overlay nil)

(defvar-local shco--current-prefix ""
  "The prefix string currently being completed.")

(defvar-local shco--current-candidate-index 0)

(defvar-local shco--possible-candidates nil
  "A list of possible completion candidates at point.")

(defun shco--reset-candidates ()
  "Run all completion at point functions until one produces candidates.

Candidates will be stored in `shco--possible-candidates'.  The
string being completed will be stored in `shco--current-prefix'."
  (setq shco--possible-candidates nil)
  (setq shco--current-candidate-index 0)
  (dolist (completion-fn completion-at-point-functions)
    (when (and (not shco--possible-candidates)
               (functionp completion-fn))
      (let ((completion-out (funcall completion-fn)))
        ;; Calling a completion func that can handle this completion should
        ;; result in the form '(start end collection . props)'.
        (when-let ((start (nth 0 completion-out))
                   (end (nth 1 completion-out))
                   (collection (nth 2 completion-out)))
          (setq shco--current-prefix (buffer-substring-no-properties start end))
          (setq shco--possible-candidates
                (all-completions shco--current-prefix collection)))))))

(defun shco--cycle-candidate ()
  "Return the next available completion candidate.

A return value of nil indicates no completions available."
  (when shco--possible-candidates
    (let ((cand (nth shco--current-candidate-index shco--possible-candidates)))
      (if (eql shco--current-candidate-index (1- (length shco--possible-candidates)))
          (setq shco--current-candidate-index 0)
        (setq shco--current-candidate-index (1+ shco--current-candidate-index)))
      cand)))

(defun shco--preview ()
  "Show preview at current point."
  (shco--reset-candidates)
  (when (>= (length shco--current-prefix) shco-min-prefix-length)
    (when-let ((cand (shco--cycle-candidate)))
      (let ((cand-str (substring cand (length shco--current-prefix))))
        (when (not (string-empty-p cand-str))
          (setq shco--preview-overlay (make-overlay (point) (point)))
          (add-text-properties 0 1 '(cursor 1) cand-str)
          (add-text-properties 0 (length cand-str) '(face shadow) cand-str)
          (overlay-put shco--preview-overlay 'after-string cand-str))))))

(defun shco--preview-remove ()
  "Remove the completion preview."
  (when shco--preview-overlay
    (delete-overlay shco--preview-overlay)
    (setq shco--preview-overlay nil)))

(defun shco--cancel-timer ()
  "Cancel the current preview timer if active."
  (when (timerp shco--preview-timer)
    (cancel-timer shco--preview-timer)
    (setq shco--preview-timer nil)))

(defun shco--pre-command-hook ()
  "Pre command hook function."
  (shco--preview-remove))

(defun shco--post-command-hook ()
  "Post command hook function."
  (shco--cancel-timer)
  (when (and shco-mode
             (not (minibufferp (current-buffer))))
    (setq shco--preview-timer
          (run-with-idle-timer
           shco-preview-delay nil #'shco--preview))))

;;;###autoload
(define-minor-mode shco-mode
  "Toggle `shco-mode'."
  :lighter "shco"
  :init-value nil
  (if shco-mode
      (progn
        (add-hook 'pre-command-hook #'shco--pre-command-hook nil t)
        (add-hook 'post-command-hook #'shco--post-command-hook nil t))
    (progn
      (remove-hook 'pre-command-hook #'shco--pre-command-hook t)
      (remove-hook 'post-command-hook #'shco--post-command-hook t)

      (shco--preview-remove)
      (shco--cancel-timer))))

(provide 'shco)
;;; shco.el ends here
