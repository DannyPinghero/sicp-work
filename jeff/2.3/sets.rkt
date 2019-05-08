(define (echo line) (display line) (newline))

; Exercise 2.59
; First, the text's mplementation of a set as an unordered list.
; This is linear in time...
(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
    (if (element-of-set? x set)  ; ... and, hence, this is too, ...
        set
        (cons x set)))
(define (intersection-set s1 s2)
    (cond ((or (null? s1) (null? s2)) '())
          ((element-of-set? (car s1) s2)  ; ... and this.
           (cons (car s1)
                 (intersection-set (cdr s1) s2)))
          (else (intersection-set (cdr s1) s2))))
; Now, my code.
; The union is pretty easy!
; We already have a set-membership check in adjoin-set.
; Leverage it:
(define (union-set s1 s2)
    (if (null? s1)
        s2
        (adjoin-set (car s1)
                    (union-set (cdr s1)
                               s2))))
; Quick tests:
(echo (union-set '() '()))
(echo (union-set '(1) '()))
(echo (union-set '() '(1)))
(echo (union-set '(1) '(1)))  ; No duplicates!
(echo (union-set '(1 2 3 4) '(3 4 5 6)))
(echo (union-set '(1 3 5 7) '(2 4 6 8)))
(echo (union-set '(2 4 6 8) '(1 3 5 7)))

; Exercise 2.60
; This is interesting, actually!
; A couple functions stays the same.
; Or, at least, can't be meaningfully reduced:
(define element-of-set? element-of-set?)
(define intersection-set intersection-set)
; Adjoin-set can be made constant-time:
(define (adjoin-set x set)
    (cons x set))
; And union-set can be made a bit more concise.
; There's even an improvement if we leave the code unchanged!
; Since adjoin-set is constant, we've gone quadratic -> linear, for free.
; But it's hard to resist:
(define (union-set s1 s2)
    (append s1 s2))
; But it's important to keep in mind: our sets are growing very large.
; Consider we want to:
;   1) Loop through every citizen in the United States, n.
;     1) Record their state of residence, k.
;     2) Check the set: have we seen this state before?
;       1) ...
;     3) Store the state in the set.
; With a non-duplicative set, the lookup in 1.2 checks, at worst, K, 50 states.
; With a duplicative set, it checks, at worst, N, something like 3e8 states.
; This particular algorithm would be quadratic, and not linear, in N.
; So, it might be said that this approach is inappropriate when
;   K) the size of our "set universe", is greatly overwhelmed by
;   N) the size of our "input universe".
; However, if K is large, and we have *very few* collisions,
; then the size difference between the two approaches won't be significant,
; and we might appreciate having those quick appends!
; (But then why use a set?)

; Exercise 2.61
; Now we move on to ordered sets.
; First, some text code:
(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((= x (car set)) #t)
          ((> x (car set)) #f)
          (else (element-of-set? x (cdr set)))))
; This code assumes we always have numbers in our set.
; (Of course, they're skirting around the issue of "hashing".)
; Now, my code.
; We have to write adjoin-set such that the ordering is preserved.
; To wit: We have to "insert" x *just before* the *first* element larger than x.
; If that element doesn't exist, x goes at the end of the list.
; Also, we have to be mindful not to *duplicate* x, if it's already in the list.
(define (adjoin-set x set)
    (if (null? set)
        (list x)
        (let ((head (car set))
              (tail (cdr set)))  ; <-- modestly wasteful.
             (cond ((= head x) set)
                   ((< head x) (cons head (adjoin-set x tail)))
                   ((> head x) (cons x set))))))
; Quick tests:
(echo (adjoin-set 1 '()))
(echo (adjoin-set 5 '(1 2 3 4)))
(echo (adjoin-set 3 '(1 2 4 5)))
(echo (adjoin-set 2 '(1 2 3 4 5)))

; Exercise 2.62
; Pretty simple; this is like a merge routine.
(define (merge l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else (let ((h1 (car l1))
                      (t1 (cdr l1))
                      (h2 (car l2))
                      (t2 (cdr l2)))
                     (cond ((= h1 h2) (cons h1 (merge t1 t2)))
                           ((< h1 h2) (cons h1 (merge t1 l2)))
                           ((> h1 h2) (cons h2 (merge l1 t2))))))))
(define union-set merge)
; Again, quick tests:
(echo (union-set '() '()))
(echo (union-set '(1) '()))
(echo (union-set '() '(1)))
(echo (union-set '(1) '(1)))  ; No duplicates!
(echo (union-set '(1 2 3 4) '(3 4 5 6)))
(echo (union-set '(1 3 5 7) '(2 4 6 8)))
(echo (union-set '(2 4 6 8) '(1 3 5 7)))

; Exercise 2.63
; First, the implementation:
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))
(define _2_16_A
        '(7
          (3
           (1
            ()
            ())
           (5
            ()
            ()))
          (9
           ()
           (11
            ()
            ()))))
(define _2_16_B
        '(3
          (1
           ()
           ())
          (7
           (5
            ()
            ())
           (9
            ()
            (11
             ()
             ())))))
(define _2_16_C
        '(5
          (3
           (1
            ()
            ())
           ())
          (9
           (7
            ()
            ())
           (11
            ()
            ()))))
; Now, the first algorithm:
(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))
; This is a pretty basic in-order walk.
; But the appends look a bit dangerous...
; Consider the tree from 2.16.A:
;       7  <-- root
;      / \
;     3   9
;    / \   \
;   1   5   11
; To calculate (tree->list-1 root), we do:
;   (append (1 3 5)
;           (cons 7
;                 (9 11)))
; Recall that (append A B) is linear w.r.t the size of A,
; since we have to iterate through A to cons its last element with B.
; For a balanced tree, this append is deceptively costly!
; At depth 0, we iterate over n/2 LHS elements.
; At depth 1, we iterate over n/4 LHS elements.
; And so on; that is, at depth i we iterate over n/2^i LHS elements.
; But wait! Sum over levels:
;   N = n/2 + n/4 + ... + n/2^i  <-- i<=log2n
;     = n(1/2 + 1/4 + ... + 1/2^i)
;     < n(1 + 1/2 + 1/4 + ...)
;     = 2n
;     = O(n)
; So, just to collect our results, we've added a linear computation.
; That's in addition to the linear expense of visiting all nodes in the walk.
(echo (tree->list-1 _2_16_A))
(echo (tree->list-1 _2_16_B))
(echo (tree->list-1 _2_16_C))
; And, the second algorithm:
(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree '()))
; This one is more efficient.
; Note that we never do anything more than a cons, which is O(1).
; Mechanically, what's happening: we build the RHS tree and pass to LHS.
; So, again, for root:
;   (copy-to-list (3 ...)
;                 (cons 7
;                       (9 11)))
; So, we append RHS first, then ourself, then LHS.
; Since it's a cons (prepend), the effect is an in-order walk.
(echo (tree->list-2 _2_16_A))
(echo (tree->list-2 _2_16_B))
(echo (tree->list-2 _2_16_C))
; Winner:
(define tree->list tree->list-2)

; Exercise 2.64
(define (list->tree elements)
    (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
             (let ((left-result (partial-tree elts left-size)))
                  (let ((left-tree (car left-result))
                        (non-left-elts (cdr left-result))
                        (right-size (- n (+ left-size 1))))
                       (let ((this-entry (car non-left-elts))
                             (right-result (partial-tree (cdr non-left-elts)
                                                         right-size)))
                            (let ((right-tree (car right-result))
                                  (remaining-elts (cdr right-result)))
                                 (cons (make-tree this-entry
                                                  left-tree
                                                  right-tree)
                                        remaining-elts))))))))
; Ok, this is fun.
; First, note that the list is sorted.
; So we can auto-balance the tree by rooting at the midpoint:
;   +---+---+   +---+---+----+
;   | 1 | 3 | 5 | 7 | 9 | 11 |
;   +---+---+   +---+---+----+
;     |       |       |
;     v       |       v
;  ++   +---+ | +---+   +----+
;  || 1 | 3 | | | 7 | 9 | 11 |
;  ++   +---+ | +---+   +----+
;         |   |   |   |   |
;         v   |   v   |   v
;      ++   ++|++   ++|++    ++
;      || 3 ||||| 7 ||||| 11 ||
;      ++   ++|++   ++|++    ++
;             |       |
;             |       |
; That is:
;             5
;            / \
;           /   \
;          /     \
;         1       9
;          \     / \
;           3   7   11
; Above, I've taken care to show the partition: (left, this, right).
; The point is that we *always* pick a "this".
; (We have to: it's the entry.)
; The left and right are either empty, or ordered lists that need to be tree'd.
; The number of steps grows linearly with the number of elements.
; That might not be obvious at first, but really we're just consing each "this".
; And each element gets to be "this" once.
; (Also, the empty-list "children" of the leaves do as well.
;  If n>1, there's at most n/2 leaves; so 2*n/2=n extra cons.
;  Still linear.)
(echo (list->tree '(1 3 5 7 9 11)))

; Exercise 2.65
; We can go ordered list->tree and tree->ordered list in O(n).
; And obvoiusly we can merge ordered lists in O(n).
; So our solution is O(n) if we restrict ourselves to these 3 operations.
(define (union-set s1 s2)
    (list->tree (merge (tree->list s1)
                       (tree->list s2))))
(define (intersection-set s1 s2)
    (define (intersection-list l1 l2 i)
        (if (or (null? l1)
                (null? l2))
            i
            (let ((h1 (car l1))
                  (t1 (cdr l1))
                  (h2 (car l2))
                  (t2 (cdr l2)))
                 (cond ((= h1 h2) (intersection-list t1 t2 (cons h1 i)))
                       ((< h1 h2) (intersection-list t1 l2 i))
                       ((> h1 h2) (intersection-list l1 t2 i))))))
    (reverse (intersection-list (tree->list s1)
                                (tree->list s2)
                                '())))
(define ALL (list->tree '(0 1 2 3 4 5 6 7 8 9)))
(define EVEN (list->tree '(0 2 4 6 8)))
(define ODD (list->tree '(1 3 5 7 9)))
(echo (union-set ALL ODD))
(echo (union-set ALL EVEN))
(echo (union-set EVEN ODD))
(echo (intersection-set ALL ODD))
(echo (intersection-set ALL EVEN))
(echo (intersection-set EVEN ODD))

; Exercise 2.66
; Mapping of key to sentinel data is just: (key data).
; Then we walk the tree.
; Like we've done... quite a few times by now.
(define (record-key entry)
    (car entry))
(define (record-data entry)
    (cadr entry))
(define (lookup given root)
    (if (null? root)
        #f
        (let ((record (entry root)))
             (let ((key (record-key record)))
                  (cond ((= given key) record)
                        ((< given key) (lookup given (left-branch root)))
                        ((> given key) (lookup given (right-branch root))))))))
(echo (lookup 11
              '((13 "harden")
                ((3 "dwyane")
                 ((0 "lillard")
                  ()
                  ())
                 ((11 "klay")
                  ()
                  ()))
                ((34 "giannis")
                 ()
                 ()))))
