(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below (beside smaller smaller) painter))))

(define right-split (split beside below))
(define up-split (split below beside))


(define (split big-op small-op)
  (define (this-op painter n)
    (if (= n 0)
        painter
        (let ((smaller (this-op painter (- n 1))))
          (big-op painter (small-op smaller smaller))))))
                      