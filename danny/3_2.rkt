; 3.9 - eh no thanks
; 3.10 - also seems not worth doing
; 3.11 eh

; 3.12
; (cdr x) => '(b)
; after mutation, cdr x => '(b c d)

; 3.13
; I assume it infinite loops


; 3.14

; I thinkk this reverses a list
; loop x = [1 2 3], y = []
; temp = 2, 3
; x = 2, 3
; loop x = 2, 3 y = 1 2 3
; temp = 3
; x = 2 3 1 2 3 
; loop x = 3 y = 2 3 1 2 3
; temp = ()
; x = 2 3 1 2 3
; loop () 2 3 1 2 3
; => 2 3 1 2 3 
; not doing it quite right but the first bit is 2 3 1, hm

