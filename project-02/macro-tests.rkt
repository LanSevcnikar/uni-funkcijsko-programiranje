#lang racket

(require "02-project.rkt")

(print "----------------------------------------------------------------")
(newline)
(print "TESTING MACRO SYSTEM")
(newline)

;; Helper to run and print
(define (run-test name code expected)
  (let ([res (fri code '())])
    (print (string-append "Testing " name "... "))
    (if (equal? res expected)
        (print "PASSED")
        (begin
          (print "FAILED")
          (newline)
          (print "Expected: ") (print expected)
          (newline)
          (print "Got:      ") (print res)))
    (newline)))

;; 1. Test greater
;; 5 > 3 -> true
(run-test "greater (5 > 3)" 
          (greater (int 5) (int 3)) 
          (true))

;; 2 > 5 -> false
(run-test "greater (2 > 5)" 
          (greater (int 2) (int 5)) 
          (false))

;; 2. Test mapping
;; map (+1) [1, 2, 3] -> [2, 3, 4]
(define list-123 (.. (int 1) (.. (int 2) (.. (int 3) (empty)))))
(define list-234 (.. (int 2) (.. (int 3) (.. (int 4) (empty)))))
(define inc-func (fun "inc" (list "x") (add (valof "x") (int 1))))

(run-test "mapping (+1) [1, 2, 3]"
          (mapping inc-func list-123)
          list-234)

;; 3. Test filtering
;; filter (<= 2) [1, 3, 2, 4] -> [1, 2]
(define list-mix (.. (int 1) (.. (int 3) (.. (int 2) (.. (int 4) (empty))))))
(define list-filtered (.. (int 1) (.. (int 2) (empty))))
(define leq2-func (fun "leq2" (list "x") (?leq (valof "x") (int 2))))

(run-test "filtering (<= 2) [1, 3, 2, 4]"
          (filtering leq2-func list-mix)
          list-filtered)

;; 4. Test folding
;; fold (+) 0 [1, 2, 3, 4] -> 10
(define list-1234 (.. (int 1) (.. (int 2) (.. (int 3) (.. (int 4) (empty))))))
(define sum-func (fun "sum" (list "x" "acc") (add (valof "x") (valof "acc"))))

(run-test "folding (+) 0 [1, 2, 3, 4]"
          (folding sum-func (int 0) list-1234)
          (int 10))

;; 5. Test rev
;; rev [1, 2, 3] -> [3, 2, 1]
(define list-321 (.. (int 3) (.. (int 2) (.. (int 1) (empty)))))

(run-test "rev [1, 2, 3]"
          (rev list-123)
          list-321)

;; 6. Test binary
;; binary 0 -> empty (based on implementation)
(run-test "binary 0"
          (binary (int 0))
          (empty))

;; binary 5 -> 101 -> [1, 0, 1] (least significant bit first? Let's check implementation behavior)
;; The implementation uses mod2 then recursive call on div2.
;; 5 % 2 = 1
;; 5 / 2 = 2
;; 2 % 2 = 0
;; 2 / 2 = 1
;; 1 % 2 = 1
;; 1 / 2 = 0 -> empty
;; Result should be 1 -> 0 -> 1 -> empty. (Little Endian / LSB first)
(define bin-5 (.. (int 1) (.. (int 0) (.. (int 1) (empty)))))

(run-test "binary 5 (101)"
          (binary (int 5))
          bin-5)

;; binary 13 -> 1101 -> [1, 1, 0, 1] (MSB first)
(define bin-13 (.. (int 1) (.. (int 1) (.. (int 0) (.. (int 1) (empty))))))

(run-test "binary 13 (1101)"
          (binary (int 13))
          bin-13)

;; binary 6 -> 110 -> [1, 1, 0] (MSB first)
(define bin-6 (.. (int 1) (.. (int 1) (.. (int 0) (empty)))))

(run-test "binary 6 (110)"
          (binary (int 6))
          bin-6)
