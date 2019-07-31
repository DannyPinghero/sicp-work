(define (echo line) (display line) (newline))
(define (echof num) (display (/ num 1.0)) (newline))  ; frac -> dec

; Exercise 3.5
(define (monte-carlo trials experiment)
    (define (iter remaining passed)
        (cond ((= 0 remaining) (/ passed trials))
              ((experiment) (iter (- remaining 1) (+ passed 1)))
              (else (iter (- remaining 1) passed))))
    (iter trials 0))
(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (* range (random)))))  ; decimal
(define (estimate-area P x1 x2 y1 y2 trials)
    (define (random-point-in-circle?)
        (P (random-in-range x1 x2)
           (random-in-range y1 y2)))
    (* (monte-carlo trials random-point-in-circle?)
       (* ( - x2 x1)
          ( - y2 y1))))

; Area of unit circle = pi * r^2 = pi * 1 = pi!
; I can get this to 2 decimal places with 10e6 trials.
; That's not bad but... I feel like it should be better?
; Have I erred?
(define (in-circle-centered-at a b r)
    (lambda (x y)
        (define (square z) (* z z))
        (<= (+ (square (- x a))
               (square (- y b)))
            (square r))))
(define UNIT (in-circle-centered-at 0 0 1))
(define RUNS '(10 100 1000 100000 1000000))
(define ESTIMATES (map (lambda (n) (estimate-area UNIT -1 1 -1 1 n)) RUNS))
(for-each echof ESTIMATES)

; Exercise 3.6
; This one is a bit interesting since we didn't actually *implement* random.
; Suppose we had: http://www.cplusplus.com/reference/random/minstd_rand/.
(define A 48271)
(define M 2147483647)
(define (minstd x) (modulo (* A x) M))
(define (rand)
    (let ((x 1))
        (lambda (op)
            (cond ((eq? op 'generate) (set! x (minstd x)) x)
                  ((eq? op 'reset) (lambda (y) (set! x y)))
                  (else "wtf")))))

(define rng (rand))
(echo (rng 'generate))
(echo (rng 'generate))
(echo (rng 'generate))
(echo (rng 'generate))
(echo (rng 'generate))
((rng 'reset) 1)
(echo (rng 'generate))
(echo (rng 'generate))
(echo (rng 'generate))
(echo (rng 'generate))
(echo (rng 'generate))
