;;; packages.el --- Packages for octave

(defun octave/init-octave-mode ()
  (use-package octave
    :mode ("\\.m\\'" . octave-mode)
    :init
    (add-hook 'octave-mode-hook
              (lambda()
                (defvar mode-map (make-sparse-keymap))
                (define-key leader-map "m" mode-map)
                (define-key mode-map "o" 'run-octave)
                (define-key mode-map "b" 'octave-send-buffer)
                (define-key mode-map "r" 'octave-send-region)
                (define-key mode-map "l" 'octave-send-line)))))

(provide '+octave)
;;; packages.el ends here
