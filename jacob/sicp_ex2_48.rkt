(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect
   (+ (xcor-vect vect1) (xcor-vect vect2))
   (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect
   (- (xcor-vect vect1) (xcor-vect vect2))
   (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scalar vect)
  (make-vect
   (* scalar (xcor-vect vect))
   (* scalar (ycor-vect vect))))

; make-segment and selectors start-segment and end-segment.  

(define (make-segment start-seg end-seg)
  (cons start-seg end-seg))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))