;;; seanmacs-flymake.el --- Flymake configuration and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Flymake configuration and helpers.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package flymake
  ;; built-in, ish. Eglot pulls in development versions.
  :demand t
  :config
  (setq flymake-start-on-save-buffer t
        flymake-no-changes-timeout 1
        flymake-fringe-indicator-position 'right-fringe)

  (define-fringe-bitmap 'sm/flymake-bitmap [#b00111000] nil nil '(center t))
  (setq flymake-note-bitmap '(sm/flymake-bitmap modus-themes-fringe-cyan)
        flymake-warning-bitmap '(sm/flymake-bitmap modus-themes-fringe-yellow)
        flymake-error-bitmap '(sm/flymake-bitmap modus-themes-fringe-red))

  (mapc #'(lambda (map)
            (evil-collection-define-key 'normal map
              "p" #'sm/pop-to-some-window
              (kbd "SPC") #'flymake-show-diagnostic))
        '(flymake-diagnostics-buffer-mode-map
          flymake-project-diagnostics-mode-map))

  (defun sm/flymake-sort-entries (entries)
    "Return a sorted list of ENTRIES where each entry is sorted by
severity descending, then line ascending."
    (sort entries
          #'(lambda (a b)
              (let* ((f (lambda (entry)
                          (cons (plist-get (car entry) :severity)
                                (plist-get (car entry) :line))))
                     (cmp-a (funcall f a))
                     (cmp-b (funcall f b)))
                (or (> (car cmp-a)
                       (car cmp-b))
                    (and (eq (car cmp-a)
                             (car cmp-b))
                         (< (cdr cmp-a)
                            (cdr cmp-b))))))))

  (advice-add 'flymake--diagnostics-buffer-entries :filter-return #'sm/flymake-sort-entries)

  :hook ((prog-mode . flymake-mode))
  :bind (:prefix "C-c f"
                 :prefix-map flymake-prefix-map
                 ("l" . flymake-show-buffer-diagnostics)
                 ("L" . flymake-show-project-diagnostics)
                 ("n" . flymake-goto-next-error)
                 ("p" . flymake-goto-prev-error)))

(defvar-local sm/flymake-processes-alist nil
  "Processes that are currently checking the buffer.

Each entry in is the form of (NAME . PROCESS) where NAME is the
symbol for the checker.")

(defun sm/flymake-kill-process-if-live (name)
  "Kill process associated with NAME if it's alive."
  (when-let (proc (alist-get name sm/flymake-processes-alist))
    (when (process-live-p proc)
      (kill-process proc))))

(defun sm/match-with-function-or-number (checker match-property)
  "Get MATCH-PROPERTY from CHECKER to match the output of a checker regexp.

If MATCH-PROPERTY is a function, it will be executed with no
arguments. Otherwise it's assumed to be a number corresponding a
capture group."
  (let ((fn-or-num (plist-get checker match-property)))
    (if (functionp fn-or-num)
        (funcall fn-or-num)
      (match-string fn-or-num))))

(defun sm/flymake-checker (name checker report-fn &rest args)
  "Run CHECKER for NAME, and call REPORT-FN with the results."
  (let ((executable (executable-find (plist-get checker :executable))))
    (unless (and executable
                 (file-executable-p executable))
      (error "Could not find executable for checker: %s" name)))

  (sm/flymake-kill-process-if-live name)

  (let ((source (current-buffer))
        (command (append (list (plist-get checker :executable))
                         (plist-get checker :args)))
        (name-str (symbol-name name)))
    (save-restriction
      (widen)
      (let ((checker-proc
             (make-process
              :name (format "%s-flymake" name-str)
              :noquery t
              :connection-type 'pipe
              :buffer (generate-new-buffer (format "*%s-flymake*" name-str))
              :command command
              :sentinel
              (lambda (proc _event)
                (when (eq 'exit (process-status proc))
                  (unwind-protect
                      (if (with-current-buffer source (eq proc
                                                          (alist-get name sm/flymake-processes-alist)))
                          (with-current-buffer (process-buffer proc)
                            (goto-char (point-min))
                            (cl-loop
                             while (search-forward-regexp
                                    (plist-get checker :regexp) nil t)
                             for msg = (sm/match-with-function-or-number checker :match-msg)
                             for (beg . end) = (let ((line (sm/match-with-function-or-number checker :match-line))
                                                     (col (sm/match-with-function-or-number checker :match-col)))
                                                 (flymake-diag-region source
                                                                      (string-to-number line)
                                                                      (string-to-number col)))
                             for type = (sm/match-with-function-or-number checker :match-type)
                             collect (flymake-make-diagnostic source
                                                              beg
                                                              end
                                                              type
                                                              msg)
                             into diags
                             finally (funcall report-fn diags)))
                        (flymake-log :warning "Canceling obsolete check %s" proc))
                    (kill-buffer (process-buffer proc))))))))
        (setf (alist-get name sm/flymake-processes-alist) checker-proc)
        (process-send-region checker-proc (point-min) (point-max))
        (process-send-eof checker-proc)))))

(cl-defmacro sm/define-flymake-checker (name &key executable args regexp match-line match-col match-type match-msg)
  "Define a checker for flymake.

NAME is the symbol for uniquely identifying a flymake checker.

EXECUTABLE is a string for the command to run. ARGS will be
passed to this command. This command needs to accept the contents
of the buffer on stdin.

REGEXP is the regexp for matching the command's output.
MATCH-LINE, MATCH-COL, MATCH-TYPE, and MATCH-MSG may be either a
capture group index for the regexp, or a function that accepts no
arguments and returns a result. Functions for MATCH-LINE and
MATCH-COL should return a string that can be converted to number
with `string-to-number'. Functions for MATCH-TYPE need to return
one of :note, :warning, or :error."
  (let* ((checker-name (symbol-name name))
         (checker-fn-sym (intern (format "sm/flymake-checker-%s" checker-name)))
         (load-fn-sym (intern (format "sm/flymake-checker-%s-load" checker-name))))
    `(progn
       (defun ,checker-fn-sym (report-fn &rest args)
         ,(format "Generated flymake checker for %s." checker-name)
         (let ((checker '(
                          :executable ,executable
                          :args ,args
                          :regexp ,regexp
                          :match-line ,match-line
                          :match-col ,match-col
                          :match-type ,match-type
                          :match-msg ,match-msg)))
           (sm/flymake-checker (quote ,name) checker report-fn args)))

       (defun ,load-fn-sym ()
         ,(format "Generated flymake checker loader for %s." checker-name)
         (add-hook 'flymake-diagnostic-functions (quote ,checker-fn-sym) nil t)))))

(provide 'seanmacs-flymake)
;;; seanmacs-flymake.el ends here
