(define (echo line) (print line) (newline))

; Exercise 2.53
(echo (list 'a 'b 'c))                ; => (a b c)
(echo (list (list `george)))          ; => ((george))
(echo (cdr '((x1 x2) (y1 y2))))       ; => ((y1 y2))
(echo (cadr '((x1 x2) (y1 y2))))      ; => (y1 y2)
(echo (pair? (car '(a short list))))  ; => #f
(let ((memq (lambda (item x)
                (cond ((null? x) #f)
                      ((eq? item (car x)) x)
                      (else (memq item (cdr x)))))))
    (echo (memq 'red '((red shoes) (blue socks))))  ; => #f
    (echo (memq 'red '(red shoes blue socks))))     ; => (red shoes blue socks)

; Exercise 2.54
(define (is-equal? a b)
    (or (and (null? a)
             (null? b))
        (and (list? a)
             (list? b)
             (is-equal? (car a) (car b))
             (is-equal? (cdr a) (cdr b)))
        (and (or (and (symbol? a)
                      (symbol? b)
                      (eq? a b))
                 (and (number? a)  ; see footnote.
                      (number? b)
                      (= a b))))))
(echo (is-equal? '() '()))                                  ; => #t
(echo (is-equal? '(this is a list) '(this is a list)))      ; => #t
(echo (is-equal? '(this is a list) '(this (is a) list)))    ; => #f
(echo (is-equal? '(this (is a) list) '(this (is a) list)))  ; => #t
(let ((x 5))
    (echo (is-equal? '(1 2 3 4 x) (list 1 2 3 4 x)))   ; => #f
    (echo (is-equal? '(1 2 3 4 5) (list 1 2 3 4 x))))  ; => #t

; Exercise 2.55
; Because:
;   (car ''abracadabra)
;     => (car (quote (quote abracadabra)))
;     => (car '(quote abracadabra))
;     => quote
; Also, for fun:
;   (cadr ''abracadabra)
;     => (cadr (quote (quote abracadabra)))
;     => (cadr '(quote abracadabra))
;     => (car '(abracadabra))
;     => abracadabra
(echo (car  ''abracadabra))
(echo (cadr ''abracadabra))

; This is -- basically -- the differentiation procedure from the text.
; We modify it throughout the exercies.
; So, I provide the "skeleton" here,
; and the implementations below.
(define (differentiate x y)  ; dx/dy
    (cond ((number? x) 0)
          ((variable? x)
           (if (same-variable? x y) 1 0))
          ((sum? x)
           (make-sum (differentiate (addend x) y)
                     (differentiate (augend x) y)))
          ((product? x)
           (let ((u (multiplier x))
                 (v (multiplicand x)))
                (make-sum (make-product u (differentiate v y))
                          (make-product v (differentiate u y)))))
          ((exponentiation? x)
           (let ((u (base x))
                 (n (exponent x)))
                (make-product n  ; We didn't vararg make-product unfortunately:
                              (make-product (make-exponentiation u (- n 1))
                                            (differentiate u y)))))
          (else (error "woah, let's keep it calc 1 here..."))))

; Symbols.
; (define (number? x) built-in...)
(define (variable? x)
    (symbol? x))
(define (same-variable? x y)
    (and (variable? x)
         (variable? y)
         (eq? x y)))
(define (=number? x y)
    (and (number? x)
         (= x y)))
; Sums.
(define (sum? x)
    (and (pair? x)
         (eq? (car x) '+)))
(define (addend x)
    (cadr x))
; Products.
(define (product? x)
    (and (pair? x)
         (eq? (car x) '*)))
(define (multiplier x)
    (cadr x))

; Exercise 2.56
; Implement:
;   d(u^n)/dx = nu^(n-1) * du/dx
; This isn't too difficult.
; It's interesting though because ** doesn't actually evaluate...
(define (make-exponentiation u n)
    (cond ((=number? n 0) 1)
          ((=number? n 1) u)
          ((and (number? u) (number? n)) (expt u n))
          (else (list '** u n))))
(define (exponentiation? x)
    (and (pair? x)
         (eq? (car x) '**)))
(define (base x)
    (cadr x))
(define (exponent x)
    (caddr x))


; Exercise 2.57
; Sums we amend.
(define (make-sum x y)
    (cond ((=number? x 0) y)
          ((or (=number? y 0)
               (and (list? y)
                    (null? y))) x)
          ((and (number? x)
                (number? y) (+ x y)))
          ; We cons to process list arguments in y.
          ; This lets us handle arity>2, since:
          ;   (list + 1 (list 2 3 4))
          ;     => (+ 1 (2 3 4))
          ;   (cons (+ (cons 1 (list 2 3 4))))
          ;     => (+ 1 2 3 4)
          (else (cons '+ (cons x y)))))
(define (augend x)
    (let ((a (caddr x))  ; Awkward...
          (b (cdddr x)))
         (if (null? b)
             a
             (make-sum a b))))
; Like:
(define sum '(+ 1 2 3 4 5))
(echo (addend sum))  ; => 1
(echo (augend sum))  ; => (+ 2 3 4 5)
(echo (make-sum 1 `()))
; And, the same for products...
(define (make-product x y)
    (cond ((or (=number? x 0)
               (=number? y 0)) 0)
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((and (number? x)
                (number? y)) (* x y))
          (else (list '* x y))))
          ;(else (cons '* (cons x y)))))
(define (multiplicand x)
    (let ((a (caddr x))
          (b (cdddr x)))
         (if (null? b)
             a
             (make-product a b))))

(echo (differentiate '(+ x x (** x x)) 'x))

; This is... somewhat satisfying.
(echo (differentiate '(+ (* x 2 5) (* x x)) 'x))
(echo (differentiate '(* x (** x 2)) 'x))
;(echo (differentiate '(+ x 3) 'x))
;(echo (differentiate '(* x y) 'x))
;(echo (differentiate '(* (* x y) (+ x 3)) 'x))
;(echo (differentiate '(** x 2) 'x))  ; => 2x
;(echo (differentiate '(* 3 (** x 3)) 'x))
