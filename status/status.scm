#!/usr/bin/guile \
-e main -s
!#
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 popen))

(define (file->string f)
  (call-with-input-file f
    (lambda (p)
      (get-string-all p))))

(define (file->number f)
  (let ((s (file->string f)))
    (string->number (string-trim-both s))))

(define (command->string command)
  (let* ((p (open-input-pipe command))
         (s (get-string-all p)))
    (close-pipe p)
    (string-trim-both s)))

(define charge-full-f "/sys/class/power_supply/BAT0/charge_full")
(define charge-now-f "/sys/class/power_supply/BAT0/charge_now")
(define charge-status-f "/sys/class/power_supply/BAT0/status")

(define (battery?)
  (file-exists? charge-full-f))

(define (battery-status)
  (let ((s (string-trim-both (file->string charge-status-f))))
    (cond ((string= "Full" s) 'full)
          ((string= "Discharging" s) 'discharging)
          ((string= "Charging" s) 'charging)
          (else '()))))

(define (battery-percent)
  (let ((full (file->number charge-full-f))
        (curr (file->number charge-now-f)))
    (floor (/ (* curr 100) full))))

(define (volume-percent)
  (command->string "vol get"))

(define (mute?)
  (let ((s (command->string "pamixer --get-mute")))
    (string=? s "true")))

(define (essid)
  (command->string "essid"))

(define (format-section section value)
  (format #f "<fc=#828282> ~A:</fc> ~A" section value))

(define (format-battery)
  (if (battery?)
    (let* ((status (battery-status))
           (icon (cond ((eq? status 'discharging) " -")
                       ((eq? status 'charging) " +")
                       (else "")))
           (s (format #f "~A%~A" (battery-percent) icon)))
      (format-section "bat" s))
    ""))

(define (format-volume)
  (let* ((mute-str (if (mute?) " (mute)" ""))
         (s (format #f "~A%~A" (volume-percent) mute-str)))
    (format-section "vol" s)))

(define (format-wifi)
  (format-section "wifi" (essid)))

(define (format-status)
  (string-trim-both
   (string-join
    (list
     (format-wifi)
     (format-volume)
     (format-battery))
    "  ")))

(define (print-loop)
  (while #t
    (display (format-status))
    (newline)
    (sleep 10)))

(define (main args)
  (setvbuf (current-output-port) 'none)
  (print-loop))
