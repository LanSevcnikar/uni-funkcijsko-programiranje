#lang racket

;; I have no idea why I did not need this when submitting 08 HW but why I suddenly need it now
;; I mean, I know what it does I guess, but still
(provide 
    false 
    true 
    int 
    .. 
    empty 
    exception
    trigger 
    triggered 
    handle
    if-then-else
    ?int 
    ?bool 
    ?.. 
    ?seq 
    ?empty 
    ?exception
    add 
    mul 
    ?leq 
    ?= 
    head 
    tail 
    ~ 
    ?all 
    ?any
    vars 
    valof 
    fun 
    proc 
    closure 
    call
    greater 
    rev 
    binary 
    filtering 
    folding 
    mapping
    fri
)

;; Types
(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)
(struct exception (exn) #:transparent)

;; Errors
(struct trigger (e) #:transparent)
(struct triggered (exn) #:transparent) ;; Mislim, da tega ne rabim razen za internal
(struct handle (e1 e2 e3) #:transparent)

;; If Else
(struct if-then-else (condition e1 e2) #:transparent)

;; Type checking
(struct ?int (e) #:transparent)
(struct ?bool (e) #:transparent)
(struct ?.. (e) #:transparent)
(struct ?seq (e) #:transparent)
(struct ?empty (e) #:transparent)
(struct ?exception (e) #:transparent)

;; Operations
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct ?= (e1 e2) #:transparent)
(struct head (e) #:transparent)
(struct tail (e) #:transparent)
(struct ~ (e) #:transparent)
(struct ?all (e) #:transparent)
(struct ?any (e) #:transparent)

;; Variables
(struct vars (name val body) #:transparent)
(struct valof (name) #:transparent)

;; Functions and Procedures
(struct fun (name args body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)  ; Internal - function closure
(struct call (f args) #:transparent)

;; Interne funkcije, namenjene pomaganju pisanju kode
;; Check if a sequence is valid (ends with empty)
(define (is-valid-seq? v)
  (cond
    [(empty? v) #t] ;; ends with empty (even if only that)
    [(..? v) (is-valid-seq? (..-e2 v))] ;; check if e2 will be correct
    [else #f])) ;;something went wrong, obv not 


;; Find free variables in an expression
(define (find-free-vars expr bound-vars)
(match expr
    [
        (valof name) 
        (if (member name bound-vars) '() (list name))]
    [
        (vars name val body)
        (let* 
            (
                [val-vars (find-free-vars val bound-vars)]
                [new-bound (if (list? name) (append name bound-vars) (cons name bound-vars))]
                [body-vars (find-free-vars body new-bound)])
            (append val-vars body-vars)
        )
    ]
    [
        (fun fname args body)
        (let 
            ([new-bound (append args (if (string=? fname "") bound-vars (cons fname bound-vars)))])
            (find-free-vars body new-bound)
        )
    ]
    [
        (proc pname body)
        (let 
            ([new-bound (if (string=? pname "") bound-vars (cons pname bound-vars))])
            (find-free-vars body new-bound)
        )
    ]
    [  
        (call f args)
        (append (find-free-vars f bound-vars)
                (apply append (map (lambda (arg) (find-free-vars arg bound-vars)) args))
        )
    ]
    [
        (.. e1 e2)
        (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars))
    ]
    [
        (trigger e) 
        (find-free-vars e bound-vars)
    ]
    [
        (handle e1 e2 e3)
        (append (find-free-vars e1 bound-vars)
                (find-free-vars e2 bound-vars)
                (find-free-vars e3 bound-vars))
    ]
    [
        (if-then-else cond e1 e2)
        (append (find-free-vars cond bound-vars)
                (find-free-vars e1 bound-vars)
                (find-free-vars e2 bound-vars))
    ]
    [
        (add e1 e2)
        (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars))
    ]
    [
        (mul e1 e2)
        (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars))
    ]
    [
        (?leq e1 e2)
        (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars))
    ]
    [
        (?= e1 e2)
        (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars))
    ]
    [
        (head e)
        (find-free-vars e bound-vars)
    ]
    [
        (tail e)
        (find-free-vars e bound-vars)
    ]
    [
        (~ e)
        (find-free-vars e bound-vars)
    ]
    [
        (?int e)
        (find-free-vars e bound-vars)
    ]
    [
        (?bool e)
        (find-free-vars e bound-vars)
    ]
    [
        (?.. e)
        (find-free-vars e bound-vars)
    ]
    [
        (?seq e)
        (find-free-vars e bound-vars)
    ]
    [
        (?empty e)
        (find-free-vars e bound-vars)
    ]
    [
        (?exception e)
        (find-free-vars e bound-vars)
    ]
    [
        (?all e)
        (find-free-vars e bound-vars)
    ]
    [
        (?any e)
        (find-free-vars e bound-vars)
    ]
    [
        else
        '()
    ]
))

;; Optimize closure environment - remove unnecessary variables
;; This is currently giving me errors
(define (optimize-closure-env env f-args f-body current-env)
  (let* ([free-vars (remove-duplicates (find-free-vars f-body f-args))])
    ;; Check for undefined variables
    (for-each
     (lambda (var)
       (unless (assoc var env)
         (raise (triggered (exception "closure: undefined variable")))))
     free-vars)
    ;; Keep only necessary variables
    (filter (lambda (binding) (member (car binding) free-vars)) env)))


;; Extend environment with new bindings
;; names and values have to be lists of the same length
(define (extend-env names values env)
    (let* 
        (
            [evaled-values (map (lambda (v) (if (triggered? v) v (fri v env))) values)] ;; evaluate all values first
            [first-triggered (findf triggered? evaled-values)] ;; check if any value is already a triggered exception
            [zipped (map cons names evaled-values)] ;; zip names and values together
            [duplicate-name? (ormap (lambda (pair) (assoc (car pair) env)) zipped)] ;; check if any name is already in env
            [duplicate-in-zipped? (ormap (lambda (n) (> (count (lambda (pair) (equal? n (car pair))) zipped) 1)) names)] ;; check if any name is duplicated within zipped itself
        )
        (cond
            [first-triggered first-triggered]  ;; propagate error if any value triggered
            [duplicate-name? (triggered (exception "extend-env: duplicate name"))]
            [duplicate-in-zipped? (triggered (exception "extend-env: duplicate name"))]
            [else (append zipped env)] ;; extend env
        )
    )       
)


;; Lookup variable in environment
(define (lookup-env name env)
    (let 
        (
            [binding (assoc name env)]
        )
        (if binding
            (cdr binding)
            (triggered (exception "valof: undefined variable"))
        )
    )
)


(define (fri expr env)
  (match expr
    ;; If expression is just a data type, return it
    [(true) (true)]
    [(false) (false)]
    [(int n) (int n)]
    [(empty) (empty)]
    [(exception s) (exception s)]
    
    ;; Triggered exceptions - propagate
    [(triggered exn) (triggered exn)]
    
    ;; Sequences
    ;; evaluate e2. If results in error, return the error
    ;; if not, eval e1, if error, return error
    ;; if not, return .. v1 v2
    ;; the way the task is worded
    ;;      "(.. e1 e2) pa zaporedje, ki ga dobimo, če rezultat evalvacije izraza e1 dodamo na začetek zaporedja, ki ga dobimo kot rezultat evalvacije izraza e2."
    ;; makes me think I have to do this opposite order but I really am not sure
    ;; TODO: check if this is correct
    [
        (.. e1 e2)
        (let 
            ([v1 (fri e1 env)])
            (if 
                (triggered? v1)
                v1
                (let 
                    ([v2 (fri e2 env)])
                    (if (triggered? v2) v2 (.. v1 v2)) ;; here I flip them back
                )
            )
        )
    ]
    
    ;; Exception handling
    ;; trigger e. E must eval to an error
    [
        (trigger e)
        (let 
            ([v (fri e env)])
            (cond
                [(triggered? v) v]
                [(exception? v) (triggered v)]
                [else (triggered (exception "trigger: wrong argument type"))]
            )
        )
    ]
    
    ;; error handling
    [
        (handle e1 e2 e3)
        (let 
            ([v1 (fri e1 env)])
            (cond
                [(triggered? v1) v1] ;; Če se izraz e1 evalvira v sproženo izjemo, je rezultat, kar ta sprožena izjema.
                [(not (exception? v1)) (triggered (exception "handle: wrong argument type"))] ;; Če se izraz e1 ne evalvira v izjemo je rezultat sprožena izjema "handle: wrong argument type" v FR.
                [
                    else ;; Če se izraz e2 evalvira v sprožena izjemo (ki ustreza izjemi e1) je rezultat evalviran izraz e3, drugače pa je rezultat evalviran izraz e2.
                    (let 
                        ([v2 (fri e2 env)])
                        (cond
                            [
                                (triggered? v2)
                                (if (equal? v1 (triggered-exn v2)) (fri e3 env) v2)
                            ]
                            [else v2]
                        )
                    )
                ]
            )
        )
    ]
    
    ;; Control flow
    ;; Če se izraz condition evalvira v (false), potem je rezultat evalviran izraz e2, v vseh drugih primerih je rezultat evalviran izraz e1.
    ;; This makes me worried that if statments should be ignored, but I think that surely is not true
    ;; IMPORTAINT, CHECK IF THIS MIGHT MAKE MORE SENSE TO IGNORE ERRORS
    ;; changing this does not cvhange then umber of correct responses but I really do think that 
    ;; it only makes sense to always propagate errors first 
    [
        (if-then-else cond e1 e2)
        (let 
            ([v-cond (fri cond env)])
            (if 
                (triggered? v-cond)
                v-cond
                (if (false? v-cond) (fri e2 env) (fri e1 env))
            )
        )
    ]
    
    ;; types have a similar problem. I am unsure if the error must propagate, the way it is worded makes me think
    ;; that I should just be returning true or false. Hopefully the distinction between triggered and exception gives
    ;; a good middleground
    [
        (?int e)
        (let 
            ([v (fri e env)])
            (if (triggered? v) v (if (int? v) (true) (false)))
        )
    ]
    
    [
        (?bool e)
        (let 
            ([v (fri e env)])
            (if (triggered? v) v (if (or (true? v) (false? v)) (true) (false)))
        )
    ]
    
    [
        (?.. e)
        (let 
            ([v (fri e env)])
            (if (triggered? v) v (if (..? v) (true) (false)))
        )
    ]
    
    [
        (?seq e)
        (let 
            ([v (fri e env)])
            (cond
                [(triggered? v) v]
                [(empty? v) (true)]
                [(..? v) (if (is-valid-seq? v) (true) (false))]
                [else (false)]
            )
        )
    ]
    
    [
        (?empty e)
        (let 
            ([v (fri e env)])
            (if (triggered? v) v (if (empty? v) (true) (false)))
        )
    ]
    
    [
        (?exception e)
        (let 
            ([v (fri e env)])
            (if (triggered? v) v (if (exception? v) (true) (false)))
        )
    ]
    
    ;; Operations, a few of them really similar to 08.rkt
    ;; ! IMPORTAINT, check if seq is defined well, I suspect that I might should be using seq! testing, not this
    [
        (add e1 e2)
        ;; this susage is just handling the errors. It would be too indented otherwise and it would
        ;; not be readable. v1 is eval, if error then error, if not then v2 is eval, if not error then op done
        (let  ([v1 (fri e1 env)]) (if (triggered? v1) v1 (let ([v2 (fri e2 env)]) (if (triggered? v2) v2
            (cond
                [
                    ;; or
                    (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                    (if (or (true? v1) (true? v2)) (true) (false))
                ]
                [
                    ;; sum
                    (and (int? v1) (int? v2))
                    (int (+ (int-n v1) (int-n v2)))
                ]
                [
                    ;; concat
                    (and (or (empty? v1) (..? v1)) (or (empty? v2) (..? v2)))
                    (if 
                        (and (is-valid-seq? v1) (is-valid-seq? v2))
                        (append-sequences v1 v2)
                        (triggered (exception "add: wrong argument type"))
                    )
                ]
                [
                    ;; error if not these 3
                    else (triggered (exception "add: wrong argument type"))
                ]
        )))))
    ]
    
    ;; basically everything is done exactly the same as it was done in 08.rkt
    ;; error handling susage is the same as before
    [
        (mul e1 e2)
        (let ([v1 (fri e1 env)]) (if (triggered? v1) v1 (let ([v2 (fri e2 env)]) (if (triggered? v2) v2
            (cond
                [
                    ;; and
                    (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                    (if (and (true? v1) (true? v2)) (true) (false))
                ]
                    ;; mul
                [
                    (and (int? v1) (int? v2))
                    (int (* (int-n v1) (int-n v2)))
                ]
                [
                    ;; no seq this time
                    else 
                    (triggered (exception "mul: wrong argument type"))
                ]
        )))))
    ]

    ;; less than or equal to, note that implic for bool, normal for int, length comp for seq    
    [
        (?leq e1 e2)
        (let ([v1 (fri e1 env)]) (if (triggered? v1) v1 (let ([v2 (fri e2 env)]) (if (triggered? v2) v2
            (cond
                [
                    ;;impl
                    (and (or (true? v1) (false? v1))(or (true? v2) (false? v2)))
                    (if (or (false? v1) (true? v2)) (true) (false))
                ]
                [
                    ;; comp
                    (and (int? v1) (int? v2))
                    (if (<= (int-n v1) (int-n v2)) (true) (false))
                ]
                [
                    ;; len comp
                    (and (or (empty? v1) (..? v1)) (or (empty? v2) (..? v2)))
                    (if 
                        (and (is-valid-seq? v1) (is-valid-seq? v2))
                        (if (<= (seq-length v1) (seq-length v2)) (true) (false))
                        (triggered (exception "?leq: wrong argument type"))
                    )
                ]
                [else (triggered (exception "?leq: wrong argument type"))
                ]
        )))))
    ]
    
    ;; equal
    ;; I hope this is strong enough!!!
    ;; might have to add new tests for this, I assume it might have some troubles checking for seq
    [
        (?= e1 e2)
        (let ([v1 (fri e1 env)]) (if (triggered? v1) v1 (let ([v2 (fri e2 env)]) (if (triggered? v2) v2
            (if (equal? v1 v2) (true) (false))
        ))))
    ]
    
    ;; head and tail
    ;; similar to one another
    ;; if triggered error, propagate, if empty throw error
    [
        (head e)
        (let 
            ([v (fri e env)])
            (cond
                [(triggered? v) v]
                [(..? v) (..-e1 v)]
                [(empty? v) (triggered (exception "head: empty sequence"))]
                [else (triggered (exception "head: wrong argument type"))]
            )
        )
    ]
    
    [
        (tail e)
        (let 
            ([v (fri e env)])
            (cond
                [(triggered? v) v]
                [(..? v) (..-e2 v)]
                [(empty? v) (triggered (exception "tail: empty sequence"))]
                [else (triggered (exception "tail: wrong argument type"))]
            )
        )
    ]
    
    ;; Negation
    ;; just the same as in HW plus propagation fo triggered
    [
        (~ e)
        (let 
            ([v (fri e env)])
            (cond
                [(triggered? v) v]
                [(true? v) (false)]
                [(false? v) (true)]
                [(int? v) (int (- (int-n v)))]
                [else (triggered (exception "~: wrong argument type"))]
            )
        )
    ]
    
    ;; all and any, both really similar. First we test if it is of type seq
    [
        (?all e)
        (let 
            ([v (fri e env)])
            (cond
                [(triggered? v) v]
                [(not (or (empty? v) (..? v))) (triggered (exception "?all: wrong argument type"))]
                [(not (is-valid-seq? v)) (triggered (exception "?all: wrong argument type"))]
                [else (check-all-true v)]
            )
        )
    ]
    
    [
        (?any e)
        (let 
            ([v (fri e env)])
            (cond
                [(triggered? v) v]
                [(not (or (empty? v) (..? v))) (triggered (exception "?any: wrong argument type"))]
                [(not (is-valid-seq? v)) (triggered (exception "?any: wrong argument type"))]
                [else (check-any-true v)]
            )
        )
    ]
    
    ;; Variables
    ;; I am not sure why we are using rocket lists instead of sequences we implemented, it feeels like 
    ;; it would make more sense to do it the other way around
    [
        (vars name val body)
        (cond
            [
                (list? name) ;; racket list
                (fri body (extend-env name val env))
            ]
            [
                else
                (fri (vars (list name) (list val) body) env) ;; pretend it is a racket list
            ]
        )
    ]
        
    
    [
        (valof name)
        (fri (lookup-env name env) env)
    ]
    
    ;; Functions and procedures