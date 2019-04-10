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
