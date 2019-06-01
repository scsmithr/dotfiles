;;; packages.el --- Packages for octave

(defun octave/init-octave-mode ()
  (use-package octave
    :mode ("\\.m\\'" . octave-mode)
    :init
    (core/local 'octave-mode
     "o" 'run-octave
     "b" 'octave-send-buffer
     "r" 'octave-send-region
     "l" 'octave-send-line)))

(provide '+octave)
;;; packages.el ends here
