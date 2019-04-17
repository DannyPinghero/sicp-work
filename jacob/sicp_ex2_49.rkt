;a.  The painter that draws the outline of the designated frame.  
(define origin-vect (make-vect 0 0))
(define top-left-corner-vect (make-vect 0 1))
(define top-right-corner-vect (make-vect 1 1))
(define bottom-right-corner-vect (make-vect 1 0))

(define left-edge (make-segment (origin-vect top-left-corner-vect)))
(define top-edge (make-segment (top-left-corner-vect top-right-corner-vect)))
(define right-edge (make-segment (top-right-corner-vect bottom-right-corner-vect)))
(define bottom-edge (make-segment (bottom-right-corner-vect origin-vect)))
(define segment-list (list left-edge top-edge right-edge bottom-edge))
(segments->painter segment-list)  

;b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
(define seg1 (make-segment origin-vect top-right-corner-vect))
(define seg2 (make-segment  bottom-right-corner-vect  top-left-corner-vect))
(define segment-list (list  seg1 seg2))
(segments->painter segment-list)  

;c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

(define left-mid (make-vect 0 0.5))
(define top-mid (make-vect 0.5 1))
(define right-mid (make-vect 1 0.5))  
(define bottom-mid (make-vect 0.5 0))
(define nw-seg (make-segment left-mid top-mid))
(define ne-seg (make-segment top-mid right-mid))
(define se-seg (make-segment right-mid bottom-mid))  
(define sw-seg (make-segment bottom-mid left-mid))
(define segment-list (list nw-seg ne-seg se-seg sw-seg))
(segments->painter segment-list)  

;d.  The wave painter.
;no