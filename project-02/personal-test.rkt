#lang racket

(require "02-project.rkt")

(print "----------------------------------------------------------------")
(newline)
(print "TESTING FIBONACCI")
(newline)

;; Fibonacci
;; fib(n) = if n <= 2 then 1 else fib(n-1) + fib(n-2)
(define fib-code
  (fun "fib" (list "n")
       (if-then-else (?leq (valof "n") (int 2))
                     (int 1)
                     (add (call (valof "fib") (list (add (valof "n") (int -1))))
                          (call (valof "fib") (list (add (valof "n") (int -2))))))))

(define k-fib 10)
(print (string-append "Calculating Fib(" (number->string k-fib) ")..."))
(newline)
(define res-fib (fri (call fib-code (list (int k-fib))) '()))
(print res-fib)
(newline)


(print "----------------------------------------------------------------")
(newline)
(print "TESTING FACTORIAL")
(newline)

;; Factorial
;; fact(n) = if n <= 1 then 1 else n * fact(n-1)
(define fact-code
  (fun "fact" (list "n")
       (if-then-else (?leq (valof "n") (int 1))
                     (int 1)
                     (mul (valof "n")
                          (call (valof "fact") (list (add (valof "n") (int -1))))))))

(define k-fact 5)
(print (string-append "Calculating Fact(" (number->string k-fact) ")..."))
(newline)
(define res-fact (fri (call fact-code (list (int k-fact))) '()))
(print res-fact)
(newline)


(print "----------------------------------------------------------------")
(newline)
(print "TESTING MUTUAL RECURSION (ODD/EVEN)")
(newline)

;; Mutual Recursion (passing functions as arguments)
;; Since we don't have letrec, we pass the 'partner' function as an argument.
;; even?(n, odd_func)
;; odd?(n, even_func)

(define (make-even-odd-test func-name n)
  (vars (list "even?" "odd?")
        (list 
         ;; even? definition
         (fun "even?" (list "n" "odd-f")
              (if-then-else (?leq (valof "n") (int 0))
                            (true)
                            (if-then-else (?leq (valof "n") (int 1))
                                          (false)
                                          (call (valof "odd-f") (list (add (valof "n") (int -1)) (valof "even?"))))))
         
         ;; odd? definition
         (fun "odd?" (list "n" "even-f")
              (if-then-else (?leq (valof "n") (int 0))
                            (false)
                            (if-then-else (?leq (valof "n") (int 1))
                                          (true)
                                          (call (valof "even-f") (list (add (valof "n") (int -1)) (valof "odd?"))))))
         )
        ;; Body
        (call (valof func-name) (list (int n) (valof (if (string=? func-name "even?") "odd?" "even?"))))))

(define tests 
  (list 
   (list "even?" 0 (true))
   (list "even?" 1 (false))
   (list "odd?" 1 (true))
   (list "odd?" 2 (false))
   (list "even?" 2 (true))
   (list "even?" 10 (true))
   (list "odd?" 11 (true))
   (list "even?" 11 (false))
   (list "odd?" 10 (false))
   (list "even?" 20 (true))
   ))

(for-each (lambda (t)
            (let* ([fname (first t)]
                   [val (second t)]
                   [expected (third t)]
                   [code (make-even-odd-test fname val)]
                   [res (fri code '())])
              (print (format "Testing ~a(~a) -> Expected: ~v, Got: ~v" fname val expected res))
              (newline)))
          tests)
