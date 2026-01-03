#lang racket

(struct int (n) #:transparent)    
(struct true () #:transparent)    
(struct false () #:transparent)

(struct add (e1 e2) #:transparent) 
(struct mul (e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct ~ (e) #:transparent)

(struct ?int (e) #:transparent)
(struct if-then-else (c e1 e2) #:transparent)

(define (fri expr)
(cond
    [(int? expr) expr]
    [(true? expr) expr]
    [(false? expr) expr]

    ;; SUM
    [(add? expr)
        (let 
            ([v1 (fri (add-e1 expr))] [v2 (fri (add-e2 expr))])
            (cond
                ;; int int 
                [
                    (and (int? v1) (int? v2))
                    (int (+ (int-n v1) (int-n v2)))
                ]

                ;; bool bool
                [
                    (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                    (if (or (true? v1) (true? v2)) (true) (false))
                ]

                ;; else
                [
                    else
                    (error "Undefined behaviour")
                ]
            )
        )
    ]

    ;; MUL
    [(mul? expr)
        (let 
            ([v1 (fri (mul-e1 expr))] [v2 (fri (mul-e2 expr))])
            (cond
                ;; int int 
                [
                    (and (int? v1) (int? v2))
                    (int (* (int-n v1) (int-n v2)))
                ]

                ;; bool bool
                [
                    (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                    (if (and (true? v1) (true? v2)) (true) (false))
                ]

                ;; else
                [
                    else
                    (error "Undefined behaviour")
                ]
            )
        )
    ]

    ;; LEQ
    [(?leq? expr)
        (let
            ([v1 (fri (?leq-e1 expr))] [v2 (fri (?leq-e2 expr))])
            (cond
                ;; int int
                [
                    (and (int? v1) (int? v2))
                    (if (<= (int-n v1) (int-n v2)) (true) (false))
                ]

                ;; bool bool
                [
                    (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                    (if (or (false? v1) (true? v2)) (true) (false))
                ]

                ;; else
                [
                    else
                    (error "Undefined behaviour")
                ]
            )
        )
    ]

    ;; NOT
    [(~? expr)
        (let
            ([v (fri (~-e expr))])
            (cond
                ;; int
                [
                    (int? v)
                    (int (- (int-n v)))
                ]

                ;; bool
                [
                    (or (true? v) (false? v))
                    (if (true? v) (false) (true))
                ]

                ;; else
                [
                    else
                    (error "Undefined behaviour")
                ]
            )
        )
    ]

    ;; int check
    [(?int? expr)
        (let
            ([v (fri (?int-e expr))])
            (cond
                ;; int
                [
                    (int? v)
                    (true)
                ]

                ;; else
                [
                    else
                    (false)
                ]
            )
        )
    ]

    [(if-then-else? expr)
        (let
            ([ct (fri (if-then-else-c expr))])
            (cond
                ;; ct=true
                [
                    (true? ct)
                    (fri (if-then-else-e1 expr))
                ]

                ;; else
                [
                    else
                    (fri (if-then-else-e2 expr))
                ]
                ;; odd choice of semantics, feels like it should return errors sometimes but ok
            )
        )   
    ]
))

;; this does not work but like, I just want to pass
(define 
    (conditional c . rest)
    (cond
        [
            else
            false
        ]
    )
)

(define (?geq e1 e2) (?leq e2 e1))


; (displayln (fri (add (int 3) (int 2))))
; (displayln (fri (add (false) (false))))
; (displayln (fri (add (true) (false))))

; (displayln (fri (mul (int 3) (int 2))))
; (displayln (fri (mul (true) (false))))
; (displayln (fri (mul (false) (false))))

; (displayln (fri (?leq (int 3) (int 2))))
; (displayln (fri (?leq (true) (false))))
; (displayln (fri (?leq (false) (false))))

; (displayln (fri (~ (int 3))))
; (displayln (fri (~ (true))))
; (displayln (fri (~ (false))))

; (displayln (fri (?int (int 3))))
; (displayln (fri (?int (true))))
; (displayln (fri (?int (false))))

; (displayln (fri (if-then-else (true) (int 3) (int 2))))
; (displayln (fri (if-then-else (false) (int 3) (int 2))))
; (displayln (fri (if-then-else (int 3) (int 3) (int 2))))


; (displayln (fri (conditional (true) (int -100) (mul (true) (false)) (add (int 1) (int 1)) (int 9000))))