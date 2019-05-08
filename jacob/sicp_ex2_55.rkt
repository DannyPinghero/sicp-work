(car ''abracadabra)
(cdr ''abracadabra)
''abc
(car (quote (quote abracadabra)))
(quote (quote abracadabra))
(quote abc)

; had to look this up a bit - the ' is syntactic sugar for (quote whatever) which is itself a list - a string of chars e.g. abc is not a list
; but (quote abc) is one - therefore ''abracadabra is actually (quote (quote abracadabra)) - the first quote makes the literal list (quote abracadabra)
; and the first element is quote - what is still strange to me is that (quote (quote abc)) evaluates to 'abc

