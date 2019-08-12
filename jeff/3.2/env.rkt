; Exercise 3.9
; This is a bit tough to render.
; (Because my screen is longer than it is wide!)
; So I will draw these vertically,
; but really they are all emanating from the enclosing environment.
; Recursive:
;   +------------+
;   | factorial: |
;   | n = 6      |
;   +------------+
;   | factorial: |
;   | n = 5      |
;   +------------+
;   | factorial: |
;   | n = 4      |
;   +------------+
;   | factorial: |
;   | n = 3      |
;   +------------+
;   | factorial: |
;   | n = 2      |
;   +------------+
;   | factorial: |
;   | n = 1      |
;   +------------+
; Iterative:
;   +---------------+
;   | factorial:    |
;   | n = 6         |
;   +---------------+
;   | fact-iter:    |
;   | product = 1   |
;   | counter = 1   |
;   | max-count = 6 |
;   +---------------+
;   | fact-iter:    |
;   | product = 1   |
;   | counter = 2   |
;   | max-count = 6 |
;   +---------------+
;   | fact-iter:    |
;   | product = 2   |
;   | counter = 3   |
;   | max-count = 6 |
;   +---------------+
;   | fact-iter:    |
;   | product = 6   |
;   | counter = 4   |
;   | max-count = 6 |
;   +---------------+
;   | fact-iter:    |
;   | product = 24  |
;   | counter = 5   |
;   | max-count = 6 |
;   +---------------+
;   | fact-iter:    |
;   | product = 120 |
;   | counter = 6   |
;   | max-count = 6 |
;   +---------------+
;   | fact-iter:    |
;   | product = 720 |
;   | counter = 7   |
;   | max-count = 6 |
;   +---------------+
; The text is correct that does "not clarify" tail recursion.
; But, kind of: it does.
; Since, in the first case, we're calling the same function over and over,
; and obviously need each intermediate result to calculate the previous.
; Whereas, in the second case, we can dispense with each intermediate result.
; (What's the value of fame{counter=6} once we have frame{counter=7}?)
