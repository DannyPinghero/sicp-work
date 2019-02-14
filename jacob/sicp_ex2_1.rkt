(define (make-rat num dem)
  (if (or (and (positive? num) (positive? dem)) (and (negative? num) (negative? dem)))
      (cons (abs num) (abs dem))
      (cons (- (abs num)) (abs dem))))

(make-rat -5 -6)
(make-rat 7 6)
(make-rat -2 3)
(make-rat 1 -6)
