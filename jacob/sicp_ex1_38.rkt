(define (cont-frac n d k)
  (define (next-term i)
    (cond ((= i k) (+ (d (- i 1)) (/ (n i) (d i))))
          ((= i 1) (/ (n i) (next-term (+ i 1))))
          (else (+ (d (- i 1)) (/ (n i) (next-term (+ i 1)))))))
  (next-term 1))


(define (n-gen i) 1)
(define (d-gen i) 
  (if (= (modulo (- i 5) 3) 0)
      (* 2 (+ 2 (/ (- i 5) 3)))
      1))

(define (whatever f k)
  ;(display k)
  (newline)
  (display (f k))
  (newline)
  (if (= k 1)
      (display "STOP")
      (whatever f (- k 1))))

;(whatever d-gen 14)

; e = 2.71828...
; continured fraction is e - 2 so
; e =
(+ 2.0 (cont-frac n-gen d-gen 10))
