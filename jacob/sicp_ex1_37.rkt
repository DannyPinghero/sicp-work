;a 
(define (cont-frac n d k)
  (define (next-term i)
    (cond ((= i k) (+ (d (- i 1)) (/ (n i) (d i))))
          ((= i 1) (/ (n i) (next-term (+ i 1))))
          (else (+ (d (- i 1)) (/ (n i) (next-term (+ i 1)))))))
  (next-term 1))

; 1/phi = 0.61803398875

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

; 12 is just enough

;b
(define (cont-frac-iter n d k)
  (define (next i previous)
    (if (= i 1)
        (/ (n i) previous)
        (next (- i 1) (+ (d (- i 1)) (/ (n i) previous)))))
  (next k (d k)))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                12)
    
  
    