(define (echo line) (display line) (newline))

; Exercise 2.7
; It's tempting to "sniff out" the proper interval endpoints:
;   (define (lower-bound i) (min (car i) (cdr i)))
;   (define (upper-bound i) (max (car i) (cdr i)))
; But this isn't in keeping e.g. with their implementation of mul-interval,
; where they've taken great pain to pass (min, max) in the correct order.
; So:
(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

; Exercise 2.8
; Have to be a bit careful here:
;   (-10, -1) - (-9, 5)
;     => (-10 - (-9), -1 - 5)
;     => (-1, -6)
; That is:
;   (lower-bound (sub-interval a b))
;     != (- (lower-bound a) (lower-bound b))
; But do we have to check every permutation? Probably not.
; Let's consider all the cases in the above example:
;   (-10, -1) - (-9, 5)
;     => ((- -10 5), (- -1 -9))
;     => (-15, 8)
; Notice, we omitted some pointless combinations, e.g.:
;   (- -10   5) <= (- -10  -9)
;   (-  -1  -9) >= (-  -1   5)
; That is:
;   small number - large number
;     <= small number - small number
; And, by symmetry:
;   large number - small number
;     >= large number - large number
(define (sub-interval a b)
    (make-interval (- (lower-bound a) (upper-bound b))
                   (- (upper-bound a) (lower-bound b))))
(define a (make-interval -10 -1))
(define b (make-interval  -9  5))
(define s (sub-interval a b))
(echo (lower-bound s))
(echo (upper-bound s))

; Exercise 2.9
(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))
; It's obviously true for addition:
;   I  = (l, u)
;   WI = (I.u - I.l) / 2
;   S  = (A.l + B.l, A.u + B.u)
;   WS = ((A.u + B.u) - (A.l + B.l)) / 2
;      = (A.u - A.l) / 2 + (B.u - B.l) / 2
;      = WA + WB
; Let's demonstrate:
(define (add-interval a b)
    (make-interval (+ (lower-bound a) (lower-bound b))
                   (+ (upper-bound a) (upper-bound b))))
(define (smart-add-width a b)
    (+ (width a) (width b)))
(define (naive-add-width a b)
    (width (add-interval a b)))
(echo (smart-add-width a b))
(echo (naive-add-width a b))
; Then suppose:
;   P  = (A.l * B.l, A.u * B.u)
;   WP = (A.u * B.u - A.l * B.l) / 2
; This is just one possible case of mul-interval,
; in which WP pretty obviously can't be decomposed in terms of WA and WB.

; Exercise 2.10
; It took me a while to understand precisely Ben's objection.
; Our solution is predicated on: Y > y => c/Y < c/y.
; This is obviously not true if y<0 and Y>0.
(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(define (div-interval x y)
    (let ((lhs (lower-bound y))
          (rhs (upper-bound y)))
        (if (and (< lhs 0) (> rhs 0))
            (echo "Ambiguous y!")  ; Actually error'ing crashes the interpreter.
            (mul-interval x (make-interval (/ 1.0 rhs)
                                           (/ 1.0 lhs))))))
(define neg (make-interval -5 -3))
(define pos (make-interval 4 5))
(define non (make-interval -2 2))
(echo (div-interval pos neg))  ; Ok!
(div-interval pos non)  ; Not ok!

; Exercise 2.11
; This problem is more interesting than it appears at first glance.
; First, let's consider all possible signs of X and Y, like a register:
;    L U | L U
;   --X--+--Y--
;    - - | - -
;    - - | - +
;    - - | + -
;    - - | + +
;    - + | - -
;    - + | - +
;    - + | + -
;    - + | + +
;    + - | - -
;    + - | - +
;    + - | + -
;    + - | + +
;    + + | - -
;    + + | - +
;    + + | + -
;    + + | + +
; Notice right away that a lot of these combinations are impossible,
; e.g. row 3, since YL < YU. So:
;    L U | L U
;   --X--+--Y--
;    - - | - -
;    - - | - +
;    - - | + +
;    - + | - -
;    - + | - +
;    - + | + +
;    + + | - -
;    + + | - +
;    + + | + +
; There's 9 cases left: we're on to something.
; Now let's keep track of what *must* be the new L and U for each case:
;      L   | L U | L U |   U
;   ---'---+--X--+--Y--+---'---
;    XU*YU | - - | - - | XL*YL
;    XL*YU | - - | - + | XL*YL
;    XL*YU | - - | + + | XU*YL
;    XU*YL | - + | - - | XL*YL
;      ?   | - + | - + |   ?
;    xL*YU | - + | + + | XU*YU
;    XU*YL | + + | - - | XL*YU
;    XU*YL | + + | - + | XU*YU
;    xL*YL | + + | + + | XU*YU
; Assuming I've made no mistakes above -- its possible! --
; we can see that the only "indeterminate" case is (-+, -+),
; since we don't know which of (XL*YU) or (XU*YL) is smaller,
; nor which or (XL*YL) or (XU*YU) is larger.
; (Intuitively, a change in sign gives us a route to the min or max.
;  For example, suppose we have all positives, and one negative.
;  Then the "path" to the new L *definitely* runs through that negative:
;  the largest positive * the one negative,
;  and the path to the new U *definitely does not* run through that negative.
;  The case of of same signdedness everywhere is "degenerate",
;  and it's obvious how to get the min or max.)
(define (neg? x) (< x 0))
(define (pos? x) (or (> x 0) (= x 0)))
(define (mul-interval x y)
    (let ((XL (lower-bound x))
          (XU (upper-bound x))
          (YL (lower-bound y))
          (YU (upper-bound y)))
        ; This could probably be written more elegantly.
        ; For example, many redundant signdedness checks.
        ; But whatever! Those are inexpensive.
        (cond ((and (neg? XL) (neg? XU) (neg? YL) (neg? YU))
               (make-interval (* XU YU) (* XL YL)))
              ((and (neg? XL) (neg? XU) (neg? YL) (pos? YU))
               (make-interval (* XL YU) (* XL YL)))
              ((and (neg? XL) (neg? XU) (pos? YL) (pos? YU))
               (make-interval (* XL YU) (* XU YL)))
              ((and (neg? XL) (pos? XU) (neg? YL) (neg? YU))
               (make-interval (* XU YL) (* XL YL)))
              ((and (neg? XL) (pos? XU) (pos? YL) (pos? YU))
               (make-interval (* XL YU) (* XU YU)))
              ((and (pos? XL) (pos? XU) (neg? YL) (neg? YU))
               (make-interval (* XU YL) (* XL YU)))
              ((and (pos? XL) (pos? XU) (neg? YL) (pos? YU))
               (make-interval (* XU YL) (* XU YU)))
              ((and (pos? XL) (pos? XU) (pos? YL) (pos? YU))
               (make-interval (* XL YL) (* XU YU)))
              ((and (neg? XL) (pos? XU) (neg? YL) (pos? YU))
               (make-interval (min (* XL YU) (* XU YL))
                              (max (* XU YU) (* XL YL)))))))
; Some random testing.
; I'm not really going to inspect this too carefully.
; I may have made some typos, etc. but the idea is correct.
(define neg1 (make-interval -7 -2))
(define neg2 (make-interval -5 -4))
(define pos1 (make-interval 2 4))
(define pos2 (make-interval 8 9))
(define non1 (make-interval -2 2))
(define non2 (make-interval -11 6))
(define (print-mul-interval x y)
    (define (print-interval i)
        (display "(")
        (display (lower-bound i))
        (display ", ")
        (display (upper-bound i))
        (display ")"))
    (print-interval x)
    (display " * ")
    (print-interval y)
    (display " = ")
    (print-interval (mul-interval x y))
    (newline))
; 6C2 is only 15, so actually I can test all of these...
; I generated this with Python; I'm not the least bit ashamed.
(print-mul-interval pos1 pos2)
(print-mul-interval pos1 neg1)
(print-mul-interval pos1 neg2)
(print-mul-interval pos1 non1)
(print-mul-interval pos1 non2)
(print-mul-interval pos2 neg1)
(print-mul-interval pos2 neg2)
(print-mul-interval pos2 non1)
(print-mul-interval pos2 non2)
(print-mul-interval neg1 neg2)
(print-mul-interval neg1 non1)
(print-mul-interval neg1 non2)
(print-mul-interval neg2 non1)
(print-mul-interval neg2 non2)
(print-mul-interval non1 non2)

; Exercise 2.12
(define (make-center-percent center tolerance)
    (let ((delta (* center tolerance)))
        (make-interval (- center delta)
                       (+ center delta))))
(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
; This one's algebraic:
;   L = C - T*C
;     => C(1 - T) = L
;     => 1 - T = L / C
;     => T = 1 - (L / c)
(define (percent i)
    (- 1 (/ (lower-bound i) (center i))))
(define R (make-center-percent 68 10))
(echo (center R))
(echo (percent R))

; Exercise 2.13
; Suppose:
;   I0 = (C0 - T0*C0, C0 + T0*C0)
;   I1 = (C1 - T1*C1, C1 + T1*C1)
; Further suppose that {T0, T1} are small.
; Then one possible product term is:
;   P01 = I0 * I1
;       = (C0 - T0*C0) * (C1 + T1*C1)
;       = C0*C1 + T1*C0*C1 - T0*C0*C1 - T0*T1*C0*C1
;      ~= C0*C1 + T1*C0*C1 - T0*C0*C1
;       = C0*C1 * (1 + T1 - T0)
; In general:
;   P = I0 * I1
;     = (C0 +- T0*C0) * (C1 +- T1*C1)
;     = C0*C1 +- T1*C0*C1 +- T0*C0*C1 +- T0*T1*C0*C1
;    ~= C0*C1 +- T1*C0*C1 +- T0*C0*C1
;     = C0*C1 * (1 +- (T1 +- T0))

