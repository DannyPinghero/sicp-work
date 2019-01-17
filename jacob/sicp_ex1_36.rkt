

(define tolerance 0.00001)


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average first second) (/ (+ first second) 2)) ; primitive but we haven't been introduced to lists yet sooooo

(define (fixed-point-dampen f first-guess) (fixed-point (lambda (x) (average x (f x)))
                                                        first-guess))

(define (func-to-find x) (/ (log 1000) (log x)))


(display "undampened")
(newline)
(fixed-point func-to-find 2)
; takes 35 steps

(display "dampened")
(newline)
(fixed-point-dampen func-to-find 2)
; takes 10 steps
