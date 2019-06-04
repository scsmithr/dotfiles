;;; packages.el --- Packages for octave

(defun octave/init-octave-mode ()
  (progn
    (use-package octave
      :defer t
      :mode ("\\.m\\'" . octave-mode))
    (defvar core-octave-mode-map (make-sparse-keymap))
    (define-key core-octave-mode-map "o" 'run-octave)
    (define-key core-octave-mode-map "sr" 'octave-send-region)
    (define-key core-octave-mode-map "sb" 'octave-send-buffer)
    (define-key core-octave-mode-map "sl" 'octave-send-line)
    (add-hook 'octave-mode-hook
              (lambda()
                (core/leader core-local-leader-key core-octave-mode-map)))))

(provide '+octave)
;;; packages.el ends here
