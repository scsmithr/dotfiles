;;; packages.el --- Packages for octave

(defun octave/init-octave-mode ()
  (progn
    (use-package octave
      :defer t
      :mode ("\\.m\\'" . octave-mode))
    (core/local 'octave-mode
                "r" 'run-octave
                "sr" 'octave-send-region
                "sb" 'octave-send-buffer
                "sl" 'octave-send-line)))

(provide '+octave)
;;; packages.el ends here
