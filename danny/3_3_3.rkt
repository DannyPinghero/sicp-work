#!/usr/bin/racket
#lang racket

(require compatibility/mlist)
(require rnrs/mutable-pairs-6)


; 3.24

(define (make-table same-key?)
    (define list mlist)
    (define cons mcons)
    (define cdr mcdr)
    (define car mcar)
    (define set-cdr! set-mcdr!)
    (define set-car! set-mcar!)
    (define caar (compose car car))
    
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

        (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
            (if subtable
                (let ((record (assoc key-2 (cdr subtable))))
                    (if record
                        (cdr record)
                        false))
                false)))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
              (if subtable
                  (let ((record (assoc key-2 (cdr subtable))))
                    (if record
                        (set-cdr! record value)
                        (set-cdr! subtable
                                  (cons (cons key-2 value)(cdr subtable)))))
                  (set-cdr! local-table
                            (cons (list key-1
                                        (cons key-2 value))
                                  (cdr local-table)))))
            'ok)
        (define (dispatch m)
          (cond ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation -- TABLE" m))))
        dispatch))



; 3.25
(define (lookup keys table)
    (define (inner keys local-table)
      (let ((first-key (car keys))
            (remaining-keys (cdr keys)))
        (let ((subtable assoc key-1 (cdr local-table)))
          (if subtable
              (if (null? remaining-keys)
                  (cdr subtable)
                  (inner remaining-keys subtable)
                  )
              #f)
            )
        )
      )
    (inner keys table))

; 3.26
; yea a binary tree sounds pretty good!

; 3.27
; for the usual reasons
