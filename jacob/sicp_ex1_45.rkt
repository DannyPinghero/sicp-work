; taken from 1.36
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average first second) (/ (+ first second) 2)) ; primitive but we haven't been introduced to lists yet sooooo

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


(define (square n)
  (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (compose f g)
  (lambda (x)
    (f (g x))))
             
(define (repeated func n)
  (if (= 1 n) 
      func
      (compose func (repeated func (- n 1)))))

(define (fourth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 3)))
                            (repeated average-damp 2)
                            1.0))

;(fourth-root 16)
(fourth-root 81)

(define (fifth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 4)))
                            (repeated average-damp 2)
                            1.0))

;(fifth-root 32)
(fifth-root 243)


(define (sixth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 5)))
                            (repeated average-damp 2)
                            1.0))

;(sixth-root 64)
(sixth-root 729)



(define (seventh-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 6)))
                            (repeated average-damp 2)
                            1.0))

(seventh-root 128)
(seventh-root 2187)


(define (eigth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 7)))
                            (repeated average-damp 3)
                            1.0))

(eigth-root 256) ;FINALLY doesn't converge with 2 average damp
(eigth-root 6561)


(define (fifteenth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 14)))
                            (repeated average-damp 3)
                            1.0))

(fifteenth-root 256)
(fifteenth-root 6561)


(define (sixteenth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y 15)))
                            (repeated average-damp 3)
                            1.0))

;(sixteenth-root 256) ;doesn't converge with 3 average damp
;(sixteenth-root 6561)

; rule appears to be powers of two - the number of damps is the floor of log base 2 of n 


(define logB2 ;stole this from the internet, but floor is built in
  (lambda (x)
    (/ (log x) (log 2))))

;(logB2 16)

(define (nth-root n)
  (lambda (x) 
    (fixed-point-of-transform (lambda (y) (/ x (fast-expt y (- n 1))))
                              (repeated average-damp (floor (logB2 n)))
                              1.0)))


((nth-root 156) 1000)

  