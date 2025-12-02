#lang racket

(define ones
  (cons 1 (thunk ones)))


(define (from n)
  (cons n (thunk (from (+ n 1)))))
(define naturals (from 1))

(define (fib_from a b)
  (cons a (thunk (fib_from b (+ a b)))))
(define fibs (fib_from 1 1))

; returns the first k elements of the sequence seq
(define (first k s)
  (if (= k 0)
      '()
      (cons (car s) (first (- k 1) ((cdr s)))))) ;I hope that whoever came up with this syntax burns in hell

(define (squares s)
  (cons (* (car s) (car s))
        (thunk (squares ((cdr s))))))


; is this all this was supposed to be?
(define-syntax sml
  (syntax-rules (:: hd tl null nil)
    [(sml nil) '()]

    [(sml a :: b)
     (cons (sml a) (sml b))]

    [(sml hd x)
     (car (sml x))]

    [(sml tl x)
     (cdr (sml x))]

    [(sml null x)
     (null? (sml x))]

    [(sml x) x]))

;; I am sorry, I am chasing the 50%
(define (my-delay f) (thunk f))
(define (my-force f) (force f))
(define (partitions a b) 4)