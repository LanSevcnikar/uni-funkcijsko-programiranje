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
    [ (trigger e)       (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (head e)          (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (tail e)          (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (~ e)             (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (?int e)          (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (?bool e)         (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (?.. e)           (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (?seq e)          (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (?empty e)        (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (?exception e)    (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (?all e)          (find-free-vars e bound-vars) ] ;; just go down, no magic
    [ (?any e)          (find-free-vars e bound-vars) ] ;; just go down, no magic
    
    
    [ (.. e1 e2)    (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars)) ] ;; just go down, no magic, but 2 times
    [ (add e1 e2)   (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars)) ] ;; just go down, no magic, but 2 times
    [ (mul e1 e2)   (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars)) ] ;; just go down, no magic, but 2 times
    [ (?leq e1 e2)  (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars)) ] ;; just go down, no magic, but 2 times
    [ (?= e1 e2)    (append (find-free-vars e1 bound-vars) (find-free-vars e2 bound-vars)) ] ;; just go down, no magic, but 2 times
    
    [ 
        (handle e1 e2 e3)
        (append (find-free-vars e1 bound-vars)
                (find-free-vars e2 bound-vars)
                (find-free-vars e3 bound-vars))
    ] ;; just go down, no magic, but 3 times
    
    [ 
        (if-then-else cond e1 e2)
        (append (find-free-vars cond bound-vars)
                (find-free-vars e1 bound-vars)
                (find-free-vars e2 bound-vars))
    ] ;; just go down, no magic, but 3 times
    
    ;; If the variable that we call is in the bound variables, it is not free (the arg will be used)
    ;; Otherwise add it to the list of free variables
    [ (valof name) (if (member name bound-vars) '() (list name)) ]

    [
        (vars name val body)
        (let* 
            (
                [val-vars 
                    (if (list? val)
                        (apply append (map (lambda (v) (find-free-vars v bound-vars)) val))
                        (find-free-vars val bound-vars))
                ] ;; If val list of es, then find free in each, otherwise find free in val
                
                [new-bound (if 
                    (list? name) 
                    (append name bound-vars) 
                    (cons name bound-vars))
                ] ;; If name list of vars, then append, otherwise cons
                [body-vars (find-free-vars body new-bound)] ;; Find free in body
            )
            (append val-vars body-vars) ;; Append val-vars and body-vars
        )
    ]

    ;; For all 3 of these it is basically the same thing
    ;; Make sure that names of functions are moving correctly
    ;; If there are args, add them to the bound variables
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
    [ else '() ]
))

;; Optimize closure environment - remove unnecessary variables
(define (optimize-closure-env env fname f-args f-body current-env)
    (let* 
        (
            [bound (if (string=? fname "") f-args (cons fname f-args))]     ;; the variables that are bound in the function, add name of function if it is not empty string
            [free-vars (remove-duplicates (find-free-vars f-body bound))]   ;; the variables that are free in the function body, remove duplicates 
        )
        ;; Check for undefined variables
        ;; To be completely honest, I am not sure why I need to check this again, I would think that the lookup-env function would do that well enough
        ;; But some of the public tests fail without this check
        ;; It might be some compiling that is expected that is more than I would naturally assume from the definition of the work
        ;; (For example, I dont think many interpreter languages care that much that quickly)
        (for-each
            (lambda (var)
                (unless (assoc var env)
                    (raise (triggered (exception "closure: undefined variable"))))
            )
            free-vars
        )
        ;; Keep only necessary variables
        (filter (lambda (binding) (member (car binding) free-vars)) env)
    )
)


;; Extend environment with new bindings
;; just add the first element of names and values to the environment and call as a pair and then continue
;; Surprised the order was not reversed but as long as it works, I suppose it's fine
(define (extend-env names values env)
  (if (null? names)
      env
      (cons (cons (car names) (car values))
            (extend-env (cdr names) (cdr values) env))))

;; Lookup variable in environment
;; if the variable is not found, raise an exception
;;
(define (lookup-env name env)
    (let 
        ( [binding (assoc name env)] ) ;; finds the first element of the list whose car is name
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
    ;; I tried having a uniform function that would, in the case it is not a list, just treat it as a list of len 1 but for some reason, that did not work
    ;; I spent way too much time trying to figure out what was happening but only now do I see that I do not need to check if all 
    ;; variables repeat, just if the string s contains duplicated variables. I feel like an idiot but I think this works now as it should
    [
        (vars name val body)
        (cond
            [
                ;; Multiple variables
                (list? name)
                (if
                    (check-duplicates name)
                    (triggered (exception "vars: duplicate identifier"))
                    (let 
                        ([vals (map (lambda (v) (fri v env)) val)])
                        ;; Check for triggered exceptions in values
                        (let 
                            ([first-triggered (findf triggered? vals)])
                            (if first-triggered first-triggered (fri body (extend-env name vals env)))
                        )
                    )
                )    
            ]
            [
                ;; Single variable
                else
                (let 
                    ([v (fri val env)])
                    (if (triggered? v) v (fri body (cons (cons name v) env)))
                )
            ]
        )
    ]
    
    [
        (valof name)
        (lookup-env name env)
    ]
    
    ;; Functions and procedures
    ;; The inner workings of with-handler are to me a bit of a blur but I think I do understand it
    ;; main point is that any errors found are propagated up instead of continuing with them
    ;; I feel like I shoould be able to write this without it but I am not exactly sure if it would be nicer
    [
        (fun fname args body)
        (if 
            (check-duplicates args)
            (triggered (exception "fun: duplicate argument identifier"))
            (with-handlers 
                ([triggered? (lambda (exn) exn)])
                (let* 
                    ([optimized-env (optimize-closure-env env fname args body env)])
                    (closure optimized-env (fun fname args body))
                )
            )
        )
    ]
    
    ;; The interpreter does not run the code when it sees it
    ;; It just returns the procedure
    ;; This is so that it can be used as an argument or any other piece of data
    [
        (proc pname body)
        (proc pname body)
    ]
    
    ;; Same here
    [
        (closure env-c f)
        (closure env-c f)
    ]
    
    [
        (call f-expr args-exprs)
        (let 
            ([f-val (fri f-expr env)]) ;; evaluate the expression e, to see what we get
            (cond
                [(triggered? f-val) f-val] ;; if it is an error, we return the error, nothing more about it
                [
                    else 
                    (let 
                        ([arg-vals (map (lambda (arg) (fri arg env)) args-exprs)]) ;; evaluate the arguments
                        (let 
                            ([first-triggered (findf triggered? arg-vals)]) ;; return the first error
                            (if first-triggered first-triggered
                                ;; HERE WE FINALLY GET TO THE ACTUAL CALLING OF THE FUNCTION
                                ;; EVERYTHING BEFORE IT IS JUST ERROR HANDLING
                                (cond
                                    [
                                        ;; Closure
                                        (closure? f-val)
                                        (let* 
                                            (
                                                [clos-env (closure-env f-val)] ;; environment of the function at the time of defining
                                                [f-def (closure-f f-val)]      ;; function definition
                                                [fname (fun-name f-def)]       ;; function name
                                                [fargs (fun-args f-def)]       ;; function arguments
                                                [fbody (fun-body f-def)]       ;; function body
                                            )
                                            (if 
                                                (not (= (length fargs) (length arg-vals)))      ;; if there are more or less args than needed
                                                (triggered (exception "call: arity mismatch"))  ;; return error
                                                (let* 
                                                    (
                                                        [env-with-func (if 
                                                            (string=? fname "") 
                                                            clos-env                            ;; if function name is empty, use the closure environment
                                                            (cons (cons fname f-val) clos-env)) ;; otherwise, add the function to the environment
                                                        ]
                                                        [new-env (extend-env fargs arg-vals env-with-func)] ;; extend environment with function arguments
                                                                                                            ;; This is done last so that it overrides function name
                                                    )
                                                    (fri fbody new-env) ;; evaluate the function body
                                                )
                                            )
                                        )
                                    ]
                                    ;; Procedure
                                    ;; I really hope I did not misunderstand that a procedure is just a fcuntion without an argument
                                    ;; But all I have read makes me really believe that but then if that is the cvase, why define it seperately
                                    [
                                        (proc? f-val)
                                        (if 
                                            (not (null? arg-vals))
                                            (triggered (exception "call: arity mismatch"))
                                            (let*  
                                                (
                                                    [pname (proc-name f-val)]       ;; procedure name
                                                    [pbody (proc-body f-val)]       ;; procedure body
                                                    [new-env (if 
                                                        (string=? pname "")         ;; if procedure name is empty, use the environment
                                                        env 
                                                        (cons (cons pname f-val) env))
                                                    ]                               ;; environment with procedure
                                                )
                                                (fri pbody new-env) ;; evaluate the procedure body
                                            )
                                        )
                                    ]
                                    [
                                        else 
                                        (triggered (exception "call: wrong argument type"))
                                    ]
                                )
                            )
                        )
                    )
                ]
            )
        )
    ]
    [
        else 
        (error "Unknown expression type" expr)
    ]
))

;; ============================================================================
;; HELPER FUNCTIONS FOR SEQUENCES
;; ============================================================================

;; Append two sequences
(define (append-sequences s1 s2)
  (cond
    [(empty? s1) s2]
    [(..? s1) (.. (..-e1 s1) (append-sequences (..-e2 s1) s2))]
    [else (error "Invalid sequence")]))

;; Get sequence length
(define (seq-length s)
  (cond
    [(empty? s) 0]
    [(..? s) (+ 1 (seq-length (..-e2 s)))]
    [else 0]))

;; Check if all elements in sequence are true (not false)
(define (check-all-true s)
  (cond
    [(empty? s) (true)]
    [(..? s)
     (let ([elem (..-e1 s)])
       (cond
         [(not (or (true? elem) (false? elem)))
          (triggered (exception "?all: wrong argument type"))]
         [(false? elem) (false)]
         [else (check-all-true (..-e2 s))]))]
    [else (triggered (exception "?all: wrong argument type"))]))

;; Check if any element in sequence is not false
(define (check-any-true s)
  (cond
    [(empty? s) (false)]
    [(..? s)
     (let ([elem (..-e1 s)])
       (cond
         [(not (or (true? elem) (false? elem)))
          (triggered (exception "?any: wrong argument type"))]
         [(not (false? elem)) (true)]
         [else (check-any-true (..-e2 s))]))]
    [else (triggered (exception "?any: wrong argument type"))]))

;; ============================================================================
;; MACRO SYSTEM
;; ============================================================================

;; greater: e1 > e2 ≡ ¬(e1 ≤ e2)
(define (greater e1 e2)
  (~ (?leq e1 e2)))

;; rev: reverse a sequence
(define (rev seq)
  (folding
   (fun "" (list "x" "acc")
        (.. (valof "x") (valof "acc")))
   (empty)
   seq))

;; binary: convert positive integer to binary sequence
(define (binary e)
  (if-then-else
   (?leq e (int 0))
   (empty)
   (vars "n" e
         (vars (list "div2" "mod2")
               (list 
                ;; div2: n -> n / 2
                (fun "div2" (list "x")
                     (if-then-else (?leq (valof "x") (int 1))
                                   (int 0)
                                   (add (int 1)
                                        (call (valof "div2")
                                              (list (add (valof "x") (int -2)))))))
                ;; mod2: n -> n % 2
                (fun "mod2" (list "x")
                     (if-then-else (?leq (valof "x") (int 1))
                                   (valof "x")
                                   (call (valof "mod2")
                                         (list (add (valof "x") (int -2)))))))
               (vars "helper"
                     (fun "helper" (list "num")
                          (if-then-else
                           (?leq (valof "num") (int 0))
                           (empty)
                           (.. (call (valof "mod2") (list (valof "num")))
                               (call (valof "helper")
                                     (list (call (valof "div2") (list (valof "num"))))))))
                     (rev (call (valof "helper") (list (valof "n")))))))))

;; mapping: map function over sequence
(define (mapping f seq)
  (vars "f" f
        (vars "map-helper"
              (fun "map-helper" (list "s")
                   (if-then-else
                    (?empty (valof "s"))
                    (empty)
                    (.. (call (valof "f") (list (head (valof "s"))))
                        (call (valof "map-helper") (list (tail (valof "s")))))))
              (call (valof "map-helper") (list seq)))))

;; filtering: filter sequence by predicate
(define (filtering f seq)
  (vars "f" f
        (vars "filter-helper"
              (fun "filter-helper" (list "s")
                   (if-then-else
                    (?empty (valof "s"))
                    (empty)
                    (vars "elem" (head (valof "s"))
                          (vars "rest" (call (valof "filter-helper")
                                             (list (tail (valof "s"))))
                                (if-then-else
                                 (call (valof "f") (list (valof "elem")))
                                 (.. (valof "elem") (valof "rest"))
                                 (valof "rest"))))))
              (call (valof "filter-helper") (list seq)))))

;; folding: left fold over sequence
(define (folding f init seq)
  (vars "f" f
        (vars "init" init
              (vars "fold-helper"
                    (fun "fold-helper" (list "acc" "s")
                         (if-then-else
                          (?empty (valof "s"))
                          (valof "acc")
                          (call (valof "fold-helper")
                                (list (call (valof "f")
                                            (list (head (valof "s"))
                                                  (valof "acc")))
                                      (tail (valof "s"))))))
                    (call (valof "fold-helper")
                          (list (valof "init") seq))))))
