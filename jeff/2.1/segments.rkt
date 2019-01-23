; Exercise 2.2
; First, consider points in the (x, y) plane:
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")")
    (newline))
; Next, consider "segments" which are lines connecting points:
(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
; Then, consider a function for finding the midpoint of a segment:
(define (midpoint-segment s)
    (define (average a b) (/ (+ a b) 2.0))
    (make-point (average (x-point (start-segment s))
                         (x-point (end-segment s)))
                (average (y-point (start-segment s))
                         (y-point (end-segment s)))))
(define straight-line (make-segment (make-point 1 5) (make-point 1 10)))
(define diagonal-line (make-segment (make-point 4 2) (make-point 5 8)))
(print-point (midpoint-segment straight-line))  ; (1, 15/2 = 7.5)
(print-point (midpoint-segment diagonal-line))  ; (9/2 = 4.5, 5)

; Exercise 2.3
; Let's devise an abstraction to represent a rectangle.
; We are going to implement this in two different ways;
; the procedures to calculate area and perimiter should work with either.
; As it happens, we can compute both based on the length and width,
; such that it suffices to expose these for each implementation:
(define (rec-area r) (* (rec-length r) (rec-width r)))
(define (rec-perimeter r) (+ (* 2 (rec-length r)) (* 2 (rec-width r))))
(define (print-rec r)
    (display "length=")
    (display (rec-length r))
    (display ", width=")
    (display (rec-width r))
    (display ", area=")
    (display (rec-area r))
    (display ", perimeter=")
    (display (rec-perimeter r))
    (newline))
; Consider a rectangle as a region bounded by 4 segments:
;  y^       
;   |       
;  7| x----x
;   | |    |
;   | |    |
;   | |    |
;  3| x----x
;   |       
;   |       
; --+--------->
;     2    6  x
; Then:
;   top    = (2, 7) -> (6, 7)
;   bottom = (2, 3) -> (6, 3)
;   left   = (2, 3) -> (2, 7)
;   right  = (6, 3) -> (6, 7)
; And:
;   length    = 4
;   width     = 4
;   area      = 16
;   perimeter = 16
; Note, we don't actually *need* all four sides to calculate length and width.
; In fact, with an additional "diagonal" argument,
; we could infer bottom=>top and left=>right.
; (We would need the additional argument because the rectangle's sides
;  are not necessarily parallel with respect to the x- and y-axis.)
; Whatever, it barely matters here...
(define (make-rec top bottom left right)
    (cons (cons top bottom) (cons left right)))
(define (rec-length r)
    (let ((top (car (car r))))
        (- (x-point (end-segment top))
           (x-point (start-segment top)))))
(define (rec-width r)
    (let ((left (car (cdr r))))
        (- (y-point (end-segment left))
           (y-point (start-segment left)))))
(define top    (make-segment (make-point 2 7) (make-point 6 7)))
(define bottom (make-segment (make-point 2 3) (make-point 6 3)))
(define left   (make-segment (make-point 2 3) (make-point 2 7)))
(define right  (make-segment (make-point 6 3) (make-point 6 7)))
(print-rec (make-rec top bottom left right))
; Consider the same rectangle, this time as a "recursive" structure;
; that is, 4 contiguous rectangles delineated by a "cross":
;  y^      
;   |      
;  7| x | x
;   |   |  
;  5| -----
;   |   |  
;  3| x | x
;   |      
;   |      
; ------------>
;   | 2 4 6   x
; Then:
;   h = (2, 5) -> (6, 5)
;   v = (4, 3) => (4 ,7)
; It's possible to devise the "corners" by taking the relevant tips of h and v,
; and the "center" by finding the midpoints of h and v.
; But we don't need to here...
(define (make-rec h v) (cons h v))
(define (rec-length r)
    (let ((h (car r)))
        (- (x-point (end-segment h))
           (x-point (start-segment h)))))
(define (rec-width r)
    (let ((v (cdr r)))
        (- (y-point (end-segment v))
           (y-point (start-segment v)))))
(define h (make-segment (make-point 2 5) (make-point 6 5)))
(define v (make-segment (make-point 4 3) (make-point 4 7)))
(print-rec (make-rec h v))

