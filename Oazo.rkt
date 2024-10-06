#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumC IdC AppC IfC LamC StringC SeqC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : ExprC] [params : (Listof ExprC)]) #:transparent)
(struct IfC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([params : (Listof Symbol)] [paramsT : (Listof Ty)] [body : ExprC]) #:transparent)
(struct StringC ([s : String]) #:transparent)
(struct SeqC ([exps : (Listof ExprC)]) #:transparent)

(struct TypedParams ([params : (Listof Symbol)] [types : (Listof Ty)]) #:transparent)

(define-type Ty (U NumT BoolT StrT VoidT ArrT FunT))
(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StrT () #:transparent)
(struct VoidT () #:transparent)
(struct ArrT () #:transparent)
(struct FunT ([paramsT : (Listof Ty)] [retT : Ty]) #:transparent)

(struct Storage ((loc : Real) (val : Value)) #:transparent)

(define-type Store (Listof Storage))
(define override-store cons)

(struct Binding ((name : Symbol) (loc : Real)) #:transparent)
(define-type Env (Listof Binding))
(define extend-env cons)

(struct TBinding ((name : Symbol) (type : Ty)) #:transparent)
(define-type TEnv (Listof TBinding))

(struct V*S ([val : Value] [sto : Store]) #:transparent)
(define-type Value (U Real Boolean String ClosV PrimV NullV ArrayV MutV))
(struct ClosV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimV ([f : ((Listof Value) -> Value)]) #:transparent)
(struct MutV ([f : ((Listof Value) Store -> V*S)]) #:transparent)
(struct NullV () #:transparent)
(struct ArrayV ([loc : Real] [len : Real]) #:transparent)

; Serialize all our possible output values to a readable String
(define (serialize [v : Value]): String 
  (match v
    [(? real? r) (format "~v" r)]
    [(? string? s) (string-append "\"" s "\"")]
    [#t "true"]
    [#f "false"]
    [(ClosV a b e) "#<procedure>"]
    [(PrimV f) "#<primop>"]
    [(ArrayV a b) "#<array>"]
    ))



; Primop +
(: prim+ ((Listof Value) -> Value))
(define (prim+ vals)
  (cond
    [(and (equal? (length vals) 2) (and (real? (first vals)) (real? (first (rest vals)))))
     (+ (cast (first vals) Real) (cast (first (rest vals)) Real))]
    [else
     (error 'prim+ "OAZO: addition input not well formed")]))



(check-equal? (prim+ (list 1 2)) 3)
(check-exn (regexp (regexp-quote "OAZO: addition input not well formed"))
           (lambda () (prim+'())))

; Primop -
(: prim- ((Listof Value) -> Value))
(define (prim- vals)
  (cond
    [(and (equal? (length vals) 2) (and (real? (first vals)) (real? (first (rest vals)))))
     (- (cast (first vals) Real) (cast (first (rest vals)) Real))]
    [else
     (error 'prim- "OAZO: subtraction input not well formed")]))

(check-equal? (prim- (list 2 1)) 1)
(check-exn (regexp (regexp-quote "OAZO: subtraction input not well formed"))
           (lambda () (prim-'())))

;primop *
(: prim* ((Listof Value) -> Value))
(define (prim* vals)
  (cond
    [(and (equal? (length vals) 2) (and (real? (first vals)) (real? (first (rest vals)))))
     (* (cast (first vals) Real) (cast (first (rest vals)) Real))]
    [else
     (error 'prim* "OAZO: multiplication input not well formed")]))

(check-equal? (prim* (list 1 2)) 2)
(check-exn (regexp (regexp-quote "OAZO: multiplication input not well formed"))
           (lambda () (prim*'())))

; Primop /
(: prim/ ((Listof Value) -> Value))
(define (prim/ vals)
  (cond
    [(and (equal? (length vals) 2) (and (real? (first vals)) (real? (first (rest vals)))))
     (if (equal? 0 (first (rest vals)))
         (error 'prim/ "OAZO: Can not divide by 0")
         (/ (cast (first vals) Real) (cast (first (rest vals)) Real)))]
    [else
     (error 'prim/ "OAZO: division input not well formed")]))

(check-equal? (prim/ (list 2 1)) 2)
(check-exn (regexp (regexp-quote "OAZO: Can not divide by 0"))
           (lambda () (prim/ (list 2 0))))
(check-exn (regexp (regexp-quote "OAZO: division input not well formed"))
           (lambda () (prim/ '())))

; Primop <=
(: primle ((Listof Value) -> Value))
(define (primle vals)
  (cond
    [(and (equal? (length vals) 2) (and (real? (first vals)) (real? (first (rest vals)))))
     (<= (cast (first vals) Real) (cast (first (rest vals)) Real))]
    [else
     (error 'primle "OAZO: one argument was not a number")]))

(check-equal? (primle (list 2 2)) #t)
(check-equal? (primle (list 2 3)) #t)
(check-equal? (primle (list 2 1)) #f)
(check-exn (regexp (regexp-quote "OAZO: one argument was not a number"))
           (lambda () (primle '())))

; num-eq? function
(: num-eq? ((Listof Value) -> Value))
(define (num-eq? vals)
  (if (equal? (length vals) 2)
      (match (first vals)
        [(? real? n1)
         (match (first (rest vals))
           [(? real? n2)
            (equal? n1 n2)]
           [other (error 'num-eq? "OAZO: passed in non-numerical arguments")])]
        [other (error 'num-eq? "OAZO: passed in non-numerical arguments")])
      (error 'numeq "OAZO: did not give 2 arguments")))


(: str-eq? ((Listof Value) -> Value))
(define (str-eq? vals)
  (if (equal? (length vals) 2)
      (match (first vals)
        [(? string? s1)
         (match (first (rest vals))
           [(? string? s2)
            (equal? s1 s2)]
           [other (error 'str-eq? "OAZO: passed in non-string arguments")])]
        [other (error 'str-eq? "OAZO: passed in non-string arguments")])
      (error 'numeq "OAZO: did not give 2 arguments")))


(: arr-eq? ((Listof Value) -> Value))
(define (arr-eq? vals)
  (if (equal? (length vals) 2)
      (match (first vals)
        [(ArrayV loc1 len1)
         (match (first (rest vals))
           [(ArrayV loc2 len2)
            (if (and (= loc1 loc2) (= len1 len2))
                #t
                #f)]
           [other (error 'arr-eq? "OAZO: passed in non-array arguments")])]
        [other (error 'arr-eq? "OAZO: passed in non-array arguments")])
      (error 'arr-eq? "OAZO: passed in incorrect number of arguments")))

(: alen ((Listof Value) -> Value))
(define (alen vals)
  (match (first vals)
    [(ArrayV loc len) len]
    )
  )

(check-equal? (num-eq? (list 2 2)) #t)
(check-equal? (num-eq?  (list 2 1)) #f)
(check-exn (regexp (regexp-quote "OAZO: passed in non-numerical arguments"))
           (lambda () (num-eq? (list 2 "hi"))))
(check-exn (regexp (regexp-quote "OAZO: passed in non-numerical arguments"))
           (lambda () (num-eq? (list "hi" 2))))
(check-exn (regexp (regexp-quote "OAZO: did not give 2 arguments"))
           (lambda () (num-eq? (list 2))))
(check-equal? (str-eq? (list "hi" "hi")) #t)
(check-equal? (str-eq? (list "hi" "yo")) #f)
(check-exn (regexp (regexp-quote "OAZO: passed in non-string arguments"))
           (lambda () (str-eq? (list 2 "hi"))))
(check-exn (regexp (regexp-quote "OAZO: passed in non-string arguments"))
           (lambda () (str-eq? (list "hi" 2))))
(check-exn (regexp (regexp-quote "OAZO: did not give 2 arguments"))
           (lambda () (str-eq? (list 2))))

(define (allocate [sto : Store] [numLoc : Real] [def : Value]) : Store
  (if (= numLoc 0)
      sto
      (allocate (override-store (Storage (+ (Storage-loc (first sto)) 1) def) sto) (- numLoc 1) def)))

(: arr ((Listof Value) Store -> V*S))
(define (arr vals sto)
      (match (first vals)
        [(? real? size)
         (match (first (rest vals))
           [(? real? default)
            (let ([new-sto (allocate sto size default)])
              (V*S
               (ArrayV
                (+ (- (Storage-loc (first new-sto)) size) 1)
                size)
               new-sto))]
           )]
        
      ))

(: get-sto-val (Real Store -> Value))
(define (get-sto-val idx sto)
  (match sto
    ['() (error 'get-sto-val "OAZO: could not find given index")]
    [other (if (= (Storage-loc (first sto)) idx)
               (Storage-val (first sto))
               (get-sto-val idx (rest sto)))]))

(: aref ((Listof Value) Store -> V*S))
(define (aref vals sto)
  
      (match (first vals)
        [(ArrayV loc len)
         
             (if (< (cast (first (rest vals)) Real) len)
                 (V*S (get-sto-val (+ loc (cast (first (rest vals)) Real)) sto) sto)
                 (error 'aref "OAZO: index out of bounds"))
             ]
        
      ))

(: replace-val (Store Real Real -> Store))
(define (replace-val sto sub-idx newval)
  (cond
    [(empty? sto) '()]
    [(= (Storage-loc (first sto)) sub-idx)
     (cons (Storage sub-idx newval) (replace-val (rest sto) sub-idx newval))]
    [else (cons (first sto) (replace-val (rest sto) sub-idx newval))]))

(: aset ((Listof Value) Store -> V*S))
(define (aset vals sto)
      (match (first vals)
        [(ArrayV loc len)
         (match (first (rest vals))
           [(? real? idx)
            (match (first (rest (rest vals)))
              [(? real? newval)
               (let ([sub-idx (+ loc idx)])
                 (if (<= sub-idx (Storage-loc (first sto)))
                     (V*S (NullV) (replace-val sto sub-idx newval))
                     (error 'aset "OAZO: index out of bounds")))]
              )]
           )]
        
      ))


; REARRANGE THIS IF MAKES IT MORE LEGIBLE
(define top-env : Env (list
                       (Binding 'aset 13)
                       (Binding 'aref 12)
                       (Binding 'arr 11)
                       (Binding 'alen 10)
                       (Binding 'arr-eq? 9)
                       (Binding 'str-eq? 8)
                       (Binding 'num-eq? 7)
                       (Binding '<= 6)
                       (Binding '/ 5)
                       (Binding '* 4)
                       (Binding '- 3)
                       (Binding '+ 2)
                       (Binding 'false 1)
                       (Binding 'true 0)))

(define top-tenv : TEnv (list
                         (TBinding 'num (NumT))
                         (TBinding 'bool (BoolT))
                         (TBinding 'str (StrT))
                         (TBinding 'void (VoidT))
                         (TBinding 'numarray (ArrT))
                         (TBinding 'aset (FunT (list (ArrT) (NumT) (NumT)) (VoidT)))
                         (TBinding 'aref (FunT (list (ArrT) (NumT)) (NumT)))
                         (TBinding 'arr (FunT (list (NumT) (NumT)) (ArrT)))
                         (TBinding 'alen (FunT (list (ArrT)) (NumT)))
                         (TBinding 'arr-eq? (FunT (list (ArrT) (ArrT)) (BoolT)))
                         (TBinding 'str-eq? (FunT (list (StrT) (StrT)) (BoolT)))
                         (TBinding 'num-eq? (FunT (list (NumT) (NumT)) (BoolT)))
                         (TBinding '<= (FunT (list (NumT) (NumT)) (BoolT)))
                         (TBinding '/ (FunT (list (NumT) (NumT)) (NumT)))
                         (TBinding '* (FunT (list (NumT) (NumT)) (NumT)))
                         (TBinding '- (FunT (list (NumT) (NumT)) (NumT)))
                         (TBinding '+ (FunT (list (NumT) (NumT)) (NumT)))
                         (TBinding 'false (BoolT))
                         (TBinding 'true (BoolT))))

(define top-sto : Store (list
                         (Storage 13 (MutV aset))
                         (Storage 12 (MutV aref))
                         (Storage 11 (MutV arr))
                         (Storage 10 (PrimV alen))
                         (Storage 9 (PrimV arr-eq?))
                         (Storage 8 (PrimV str-eq?))
                         (Storage 7 (PrimV num-eq?))
                         (Storage 6 (PrimV primle))
                         (Storage 5 (PrimV prim/))
                         (Storage 4 (PrimV prim*))
                         (Storage 3 (PrimV prim-))
                         (Storage 2 (PrimV prim+))
                         (Storage 1 #f)
                         (Storage 0 #t)))


(check-equal? (allocate top-sto 5 #f)
              (append
               (list
                (Storage 18 #f)
                (Storage 17 #f)
                (Storage 16 #f)
                (Storage 15 #f)
                (Storage 14 #f))
               top-sto))
(check-equal? (arr '(5 5) top-sto)
              (V*S
               (ArrayV 14 5)
               (append
                (list
                 (Storage 18 5)
                 (Storage 17 5)
                 (Storage 16 5)
                 (Storage 15 5)
                 (Storage 14 5))
                top-sto)))
(define test-arr-1 (V*S-val (arr '(5 5) top-sto)))
(define test-sto-1 (V*S-sto (arr '(5 5) top-sto)))
(define test-arr-2 (V*S-val (arr '(5 5) top-sto)))
(define test-sto-2 (V*S-sto (arr '(5 5) top-sto)))
(define test-arr-3 (V*S-val (arr '(4 3) top-sto)))
(check-equal? (arr-eq? (list test-arr-1 test-arr-2)) #t)
(check-equal? (arr-eq? (list test-arr-1 test-arr-3)) #f)
(check-exn (regexp (regexp-quote "OAZO: passed in non-array arguments"))
           (lambda () (arr-eq? (list test-arr-1 5))))
(check-exn (regexp (regexp-quote "OAZO: passed in non-array arguments"))
           (lambda () (arr-eq? (list 5 test-arr-1))))
(check-exn (regexp (regexp-quote "OAZO: passed in incorrect number of arguments"))
           (lambda () (arr-eq? (list test-arr-1 test-arr-2 test-arr-3))))
(check-equal? (aref (list test-arr-1 2) test-sto-1)
              (V*S
               5
               (append
                (list
                 (Storage 18 5)
                 (Storage 17 5)
                 (Storage 16 5)
                 (Storage 15 5)
                 (Storage 14 5))
                top-sto)))
(check-exn (regexp (regexp-quote "OAZO: index out of bounds"))
           (lambda () (aref (list test-arr-1 5) test-sto-1)))
(check-equal? (alen (list test-arr-1)) 5)
(check-equal? (aset (list test-arr-1 4 100) test-sto-1)
              (V*S
               (NullV)
               (append
                (list
                 (Storage 18 100)
                 (Storage 17 5)
                 (Storage 16 5)
                 (Storage 15 5)
                 (Storage 14 5))
                top-sto)))
(check-exn (regexp (regexp-quote "OAZO: index out of bounds"))
           (lambda () (aset (list test-arr-1 5 100) test-sto-1)))


; Looks up a binding in the given environment
(define (lookup [for : Symbol] [env : Env]) : Real
  (match env
    ['() (error 'lookup "OAZO: Could not find name ~e in env" for)]
    [(cons (Binding name loc) r) (cond
                                   [(symbol=? for name) loc]; CHANGE THIS TO equal?
                                   [else (lookup for r)])]))

(check-equal? (lookup '+ top-env) 2)

; Looks up binding in given type environment
(define (tlookup [for : Symbol] [tenv : TEnv]) : Ty
  (match tenv
    ['() (error 'tlookup "OAZO: Could not find name ~e in tenv" for)]
    [(cons (TBinding name type) r) (cond
                                     [(symbol=? for name) type]; CHANGE THIS TO equal?
                                     [else (tlookup for r)])]))

(check-exn (regexp (regexp-quote "OAZO: Could not find name 'z in tenv"))
           (lambda () (tlookup 'z top-tenv)))

; fetches binding from store based on location passed in
(define (fetch [for : Real] [sto : Store]) : Value
  (match sto
    ['() (error 'fetch "OAZO: Could not find index ~e in store" for)]
    [(cons (Storage loc val) r) (cond
                                  [(= for loc) val]
                                  [else (fetch for r)])]))

; Determines if a symbol is an operator, returns #f otherwise
(: is-op? (-> Sexp Boolean))
(define (is-op? s)
  (cond
    [(eq? s '+) #t]
    [(eq? s '-) #t]
    [(eq? s '*) #t]
    [(eq? s '/) #t]
    [(eq? s '<=) #t]
    [(eq? s 'num-eq?) #t]
    [(eq? s 'str-eq?) #t]
    [else  #f]))

; Checks expression for if/vars out of place
(: check-exps-if-var ((Listof Sexp) -> Boolean))
(define (check-exps-if-var exps)
  (or (null? exps)
      (and (not (member (first exps) '(if then else let anon : <-)))
           (check-exps-if-var (rest exps)))))

; Retrieves all the Sexp's except for the last one in a list of Sexp's
(: get-args ((Listof Sexp) -> (Listof Sexp)))
(define (get-args lst)
  (take lst (- (length lst) 1)))

; Retrieves the last Sexp in a list of Sexp
(: get-let-body ((Listof Sexp) ->  Sexp))
(define (get-let-body lst)
  (last lst))

; Appends S-exps for easier parsing
(: concat-sexps (Sexp Sexp -> Sexp))
(define (concat-sexps sexp1 sexp2)
  (append (list sexp1) (list sexp2)))

; Check if a list of symbol are valid IdC's
(: is-valid-IdC? ((Listof Symbol) -> Boolean))
(define (is-valid-IdC? symbols)
  (andmap (lambda (s) (not (member s '(let <- : := if then else seq)))) symbols))

(: parse-type (Sexp -> Ty))
(define (parse-type s)
  (match s
    ['num (NumT)]
    ['str (StrT)]
    ['bool (BoolT)]
    ['numarray (ArrT)]
    [(list types ... '-> ret) (FunT (map (lambda ([sym : Symbol])
                                           (parse-type sym))
                                         (cast types (Listof Symbol)))
                                    (parse-type ret))]
    [other (error 'parse-type "OAZO: function parameters not well-formed")]))

; Returns typed parameters of a bound let variable
(: get-typed-let-params ((Listof Sexp) -> TypedParams))
(define (get-typed-let-params param-decs)
  (cond
    [(empty? param-decs) (TypedParams '() '())]
    [else
     (match (first param-decs)
       [(list (? symbol? p) ': t)
        (let ([next-params (get-typed-let-params (rest param-decs))]
              [param-type (parse-type t)])
          (TypedParams (cons p (TypedParams-params next-params)) (cons param-type (TypedParams-types next-params))))]
       [other (error 'parse-type "OAZO: function parameters not well-formed")])]))

; Returns typed parameters of an anonymous function
(: get-typed-anon-params ((Listof Sexp) -> TypedParams))
(define (get-typed-anon-params param-decs)
  (cond
    [(empty? param-decs) (TypedParams '() '())]
    [else
     (match (first param-decs)
       [(list t (? symbol? p))
        (let ([next-params (get-typed-anon-params (rest param-decs))]
              [param-type (parse-type t)])
          (TypedParams (cons p (TypedParams-params next-params)) (cons param-type (TypedParams-types next-params))))]
       [other (error 'parse-type "OAZO: function parameters not well-formed")])]))

; Parses a given S-expression to an ExprC
(: parse (-> Sexp ExprC))
(define (parse s)
  (match s
    [(list 'if e1 'then e2 'else e3) (IfC (parse e1) (parse e2) (parse e3))]
    [(? real? r) (NumC r)]
    [(? string? s) (StringC s)]
    [(list 'anon (list s ...) ': body)
     (let ([typed-params (get-typed-anon-params s)])
       (if (and (not (check-duplicates (TypedParams-params typed-params)))
                (is-valid-IdC? (TypedParams-params typed-params)))
           (LamC (TypedParams-params typed-params)
                 (TypedParams-types typed-params)
                 (parse body))
           (error 'parse "OAZO: contains duplicates")))]
    [(cons 'let rest)
     (match (concat-sexps (get-args (cast rest (Listof Sexp))) (get-let-body (cast rest (Listof Sexp))))
       [(list (list (list ids '<- args)...) body)
        (let ([typed-params (get-typed-let-params (cast ids (Listof Sexp)))])
          (if (and (not (check-duplicates ids)) (is-valid-IdC? (TypedParams-params typed-params)))
              (AppC (LamC
                     (TypedParams-params typed-params)
                     (TypedParams-types typed-params)
                     (parse body))
                    (map (lambda ([e : Sexp])
                           (parse e))
                         (cast args (Listof Sexp))))
              (error 'parse "OAZO: Invalid variable definitions")))]
       [other (error 'parse "OAZO: Issue with parsing variable definitions")])]
    [(? symbol? s)
     (cond
       [(is-valid-IdC? (list s))(IdC s)]
       [else (error 'parse "OAZO: Issue with parsing variable definitions")]
       )]
    [(cons 'seq exps)
     (SeqC (map (lambda ([x : Sexp])
                  (parse x))
                (cast exps (Listof Sexp))))]
    [(list exp exps ...)
     (if (not (check-exps-if-var exps))
         (error 'parse "OAZO: parse input not well-formed 1")
         (AppC (parse exp) (map (lambda ([x : Sexp])
                                  (parse x))
                                exps))
         )
     ]
    [other (error 'parse "OAZO: parse input not well-formed 4 ~v" s)]))


(define (tbind [lst : (Listof Symbol)] [types : (Listof Ty)] [tenv : TEnv]) : TEnv
  (cond
    [(empty? lst) tenv] 
    [else (tbind (rest lst) (rest types) (extend-env (TBinding (first lst) (first types)) tenv))]))

(: type-check (-> ExprC TEnv Ty))
(define (type-check expr tenv)
  (match expr
    [(NumC n) (NumT)]
    [(StringC n) (StrT)]
    [(IdC n) (tlookup n tenv)]
    [(SeqC s) (last (map (lambda ([e : ExprC])
                           (type-check e tenv))
                         s))]
    [(IfC if then else) (let ([args (list if then else)])
                          (last (map (lambda ([e : ExprC])
                                       (type-check e tenv))
                                     args)))]
    [(AppC fun args) (let
                         ([ft (type-check fun tenv)]
                          [at (map (lambda ([e : ExprC])
                                     (type-check e tenv))
                                   args)])
                       (match ft
                         [(FunT pt rt)
                          (if (equal? pt at)
                              rt
                              (error 'type-check "OAZO: TYPE ISSUE BUDDYYY"))]
                         [other (error 'type-check "OAZO: TYPE ISSUE BUDDYYY")]
                         ))]
    [(LamC params pt body)
     (FunT pt (type-check body (tbind params pt tenv)))]))



; Bind a symbol to a value in our environment
(define (bind [start-loc : Real][lst : (Listof Symbol)][env : Env]) : Env
  (cond
    [(empty? lst) env]
    [else (bind (+ start-loc 1)(rest lst) (extend-env (Binding (first lst) (+ start-loc 1)) env))]))

(define (store [vals : (Listof Value)][sto : Store]) : Store
  (cond
    [(empty? vals) sto]
    [else (store (rest vals) (override-store (Storage (+ (Storage-loc (first sto)) 1) (first vals) ) sto))]))

; Interprets the given function and returns the resolved value utilizing its list of functions
(define (interp [exp : ExprC] [env : Env] [sto : Store]) : V*S
  (match exp
    [(NumC n) (V*S n sto)]
    [(StringC s) (V*S s sto)]
    [(IdC n) (V*S (fetch (lookup n env) sto) sto)]
    [(LamC a b c) (V*S (ClosV a c env) sto)]
    [(SeqC lst)
     (let ([vals (interp-appc-args lst env sto)])
       (V*S (last (get-appc-arg-values vals)) (get-last-appc-arg-store vals)))]
    [(IfC test then else) (let ([vals (interp-appc-args (list test then else) env sto)])
                            (match (V*S-val (first vals))
                              [(? boolean? b)
                               (if b
                                   (V*S (first (rest (get-appc-arg-values vals)))
                                        (get-last-appc-arg-store vals))
                                   (V*S (first (rest (rest (get-appc-arg-values vals))))
                                        (get-last-appc-arg-store vals)))]
                              [other (error 'interp "OAZO: if test does not produce a boolean")]))]
    [(AppC fun args) 
     (let ([fn-call (interp fun env sto)])
       (match (V*S-val fn-call)
         [(PrimV f)
          (let ([vals (interp-appc-args args env (V*S-sto fn-call))])
            (V*S (f (get-appc-arg-values vals)) (get-last-appc-arg-store vals)))]
         [(ClosV params body clo-env)
          (let ([vals (interp-appc-args args env (V*S-sto fn-call))])
            
             
             (interp body
                     (bind (Storage-loc (first (get-last-appc-arg-store vals))) params clo-env)
                     (store (get-appc-arg-values vals) (get-last-appc-arg-store vals)))
             )]
         [(MutV f)
          (let ([vals (interp-appc-args args env (V*S-sto fn-call))])
            (f (get-appc-arg-values vals) (get-last-appc-arg-store vals)))]))]))

(define (interp-appc-args [exps : (Listof ExprC)][env : Env][sto : Store]) : (Listof V*S)
  (if (empty? exps)
      (cons (V*S (NullV) sto) '())
      (let ([cur-vs (interp (first exps) env sto)])
        (cons cur-vs (interp-appc-args (rest exps) env (V*S-sto cur-vs))))))

(define (get-appc-arg-values [lst : (Listof V*S)]) : (Listof Value)
  (if (= (length lst) 1)
      '()
      (cons (V*S-val (first lst)) (get-appc-arg-values (rest lst)))))

(define (get-last-appc-arg-store [lst : (Listof V*S)]) : Store
  (if (= (length lst) 1)
      (V*S-sto (first lst))
      (get-last-appc-arg-store (rest lst))))

; Combines parsing and evaluation
(: top-interp (-> Sexp String))
(define (top-interp fun-sexps)
  (let ([parsed-prog (parse fun-sexps)])
    (begin
      (type-check parsed-prog top-tenv)
      (serialize (V*S-val (interp parsed-prog top-env top-sto))))))





(check-equal? (type-check (parse '{let
                                      {[y : num] <- {+ 9 14}}
                                    {[z : num] <- 98}
                                    {+ z y}}) top-tenv) (NumT))
(check-equal? (type-check (NumC 0) top-tenv) (NumT))
(check-equal? (type-check (StringC "hi") top-tenv) (StrT))
(check-equal? (type-check (AppC (IdC '+) (list (NumC 2) (NumC 2))) top-tenv) (NumT))
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (type-check (AppC (IdC '+) (list (NumC 2))) top-tenv)))
(check-equal? (type-check (LamC '(x y) (list (NumT) (NumT)) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) top-tenv)
              (FunT (list (NumT) (NumT)) (NumT)))
(check-equal? (bind 12 '(a b c d) top-env)
              (append
               (list
                (Binding 'd 16)
                (Binding 'c 15)
                (Binding 'b 14)
                (Binding 'a 13))
               top-env))
(check-equal? (store '(0 1 2 3) top-sto)
              (append
               (list
                (Storage 17 3)
                (Storage 16 2)
                (Storage 15 1)
                (Storage 14 0))
               top-sto))

(check-equal? (serialize 2) "2")
(check-equal? (serialize "2") "\"2\"")
(check-equal? (serialize #t) "true")
(check-equal? (serialize #f) "false")
(check-equal? (serialize (ClosV '() (NumC 1) top-env)) "#<procedure>")
(check-equal? (serialize (PrimV prim+)) "#<primop>")

(check-equal? (interp-appc-args (list (NumC 1) (NumC 2)) top-env top-sto)
              (list (V*S 1 top-sto) (V*S 2 top-sto) (V*S (NullV) top-sto)))

(check-equal? (parse-type 'num) (NumT))
(check-equal? (parse-type 'str) (StrT))
(check-equal? (parse-type 'bool) (BoolT))
(check-equal? (parse-type '{num num -> num}) (FunT (list (NumT) (NumT)) (NumT)))
(check-exn (regexp (regexp-quote "OAZO: function parameters not well-formed"))
           (lambda () (parse-type 'z)))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                                       {[x : numarray] <- {arr 4 99}}
                                     {seq
                                      {aset x 2 100}
                                      {+ {+ {aref x 0} {aref x 2}} {alen x 2}}}})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                                       {[x : numarray] <- {arr 4 99}}
                                     {seq
                                      {aset x 2 100}
                                      {+ {+ {aref x 0} {aref x 2}} {alen 2}}}})))


(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                                       {[arr1 : numarray] <- {arr "hi" 0}}
                                     {[arr2 : numarray] <- {arr 4 0}}
                                     {arr-eq? arr1 arr2}})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                                       {[arr1 : numarray] <- {arr 0 "hi"}}
                                     {[arr2 : numarray] <- {arr 4 0}}
                                     {arr-eq? arr1 arr2}})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                                       {[arr1 : numarray] <- {arr 0}}
                                     {[arr2 : numarray] <- {arr 4 0}}
                                     {arr-eq? arr1 arr2}})))

(check-exn (regexp (regexp-quote "OAZO: could not find given index"))
           (lambda () (get-sto-val 1000 top-sto)))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                                       {[x : numarray] <- {arr 4 99}}
                                     {seq
                                      {aset x 2 100}
                                      {+ {+ {aref 0 0} {aref x 2}} {alen x}}}})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                                       {[x : numarray] <- {arr 4 99}}
                                     {seq
                                      {aset x 2 100}
                                      {+ {+ {aref x 0} {aref x x}} {alen x}}}})))

(check-equal? (parse `{anon {[num x] [num y]} : {+ x y}})
              (LamC '(x y) (list (NumT) (NumT))(AppC (IdC '+) (list (IdC 'x) (IdC 'y)))))
(check-equal? (parse '{seq "" "x" 0}) (SeqC (list (StringC "") (StringC "x") (NumC 0))))
(check-equal? (parse '{let
                          {[y : num] <- {+ 9 14}}
                        {[z : num] <- 98}
                        {+ z y}})
              (AppC (LamC '(y z) (list (NumT) (NumT)) (AppC (IdC '+) (list (IdC 'z) (IdC 'y))))
                    (list (AppC (IdC '+) (list (NumC 9) (NumC 14))) (NumC 98))))



(check-exn (regexp (regexp-quote "OAZO: Invalid variable definitions"))
           (lambda () (parse `(parse '{let
                                          {[z : num] <- {+ 9 14}}
                                        {[z : num] <- 98}
                                        {+ z y}}))))
(check-exn (regexp (regexp-quote "OAZO: Issue with parsing variable definitions"))
           (lambda () (parse '{let
                                  {z {+ 9 14}}
                                {y <- 98}
                                {+ z y}})))
(check-exn (regexp (regexp-quote "OAZO: parse input not well-formed"))
           (lambda () (parse `{/ then 4})))
(check-equal? (parse `{f}) (AppC (IdC 'f) '()))

(check-exn (regexp (regexp-quote "OAZO: contains duplicates"))
           (lambda () (parse `{anon {[num x] [num x]} : {+ x y}})))
(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{* 1 2}) (AppC (IdC '*) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{- 1 2}) (AppC (IdC '-) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{/ 1 2}) (AppC (IdC '/) (list (NumC 1) (NumC 2))))
(check-equal? (parse '1) (NumC 1))
(check-equal? (parse 'a) (IdC 'a))
(check-exn (regexp (regexp-quote "OAZO: function parameters not well-formed"))
           (lambda () (parse '{anon {3 4 5} : 3})))
(check-equal? (parse '{if x then x else {- x 2}}) (IfC (IdC 'x) (IdC 'x) (AppC (IdC '-) (list (IdC 'x) (NumC 2)))))
(check-exn (regexp (regexp-quote "OAZO: parse input not well-formed"))
           (lambda () (parse '{})))
(check-exn (regexp (regexp-quote "OAZO: parse input not well-formed 1"))
           (lambda () (parse '(let (c <- 5)))))
(check-exn (regexp (regexp-quote "OAZO: Issue with parsing variable definitions"))
           (lambda () (parse '(if : then 0 else 1))))
(check-exn (regexp (regexp-quote "OAZO: function parameters not well-formed"))
           (lambda () (parse '(let (: <- "") "World"))))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                                       {[x : numarray] <- {arr 4 99}}
                                     {seq
                                      {aset x 2 100}
                                      {+ {+ {aref x 0} {aref x}} {alen x}}}})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                               {[x : numarray] <- {arr 4 99}}
                             {seq
                              {aset 2 2 100}
                              {+ {+ {aref x 0} {aref x 2}} {alen x}}}})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                               {[x : numarray] <- {arr 4 99}}
                             {seq
                              {aset x x 100}
                              {+ {+ {aref x 0} {aref x 2}} {alen x}}}})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                               {[x : numarray] <- {arr 4 99}}
                             {seq
                              {aset x 2 x}
                              {+ {+ {aref x 0} {aref x 2}} {alen x}}}})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{let
                               {[x : numarray] <- {arr 4 99}}
                             {seq
                              {aset x}
                              {+ {+ {aref x 0} {aref x 2}} {alen x}}}})))


(check-equal? (top-interp '{+ 2 2}) "4")
(check-equal? (top-interp '{anon {[num x] [num y]} : {+ x y}})
              "#<procedure>")
(check-equal? (top-interp '{str-eq? "hi" "hi"}) "true")
(check-equal? (top-interp '{if {<= 2 2} then 2 else 5})
              "2")
(check-equal? (top-interp '{if true then + else -})
              "#<primop>")
(check-exn (regexp (regexp-quote "OAZO: if test does not produce a boolean"))
           (lambda () (top-interp '{if 2 then 2 else 5})))
(check-equal? (top-interp '{if {num-eq? 2 3} then 2 else 5})
              "5")
(check-equal? (top-interp `{anon {[num x] [num y]} : {+ x y}})
              "#<procedure>")
(check-equal? (top-interp '{seq 2 {+ 2 2}}) "4")
(check-equal? (top-interp '{let {[true : bool] <- false}
                             {if true then 0 else 9}})
              "9")



(check-equal? (top-interp '{let {[/ : bool] <- false}
                             {if / then 0 else 9}})
              "9")
(check-equal? (top-interp `{{anon {[num z] [num y]} : {+ z y}}
                            {/ 14 14}
                            98})
              "99")
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp `{{anon {[num z] [num y]} : {+ z y}}
                                    {+ 9 14 1}
                                    98})))

(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp `{3 4 5})))


(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp `{{anon {[num z] [num y]} : {+ z y}}
                                    {+ 9 14}
                                    98
                                    27})))
(check-exn (regexp (regexp-quote "OAZO: Can not divide by 0"))
           (lambda () (top-interp `{{anon {[num z] [num y]} : {+ z y}}
                                    {/ 9 0}
                                    98})))
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp `{{anon {} : {1}} 3 4})))
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{{{anon {} : 3}} 4 5})))




(check-equal? (top-interp '{let {[/ : bool] <- false}
                             {if / then 0 else 9}})
              "9")
(check-equal? (top-interp `{{anon {[num z] [num y]} : {+ z y}}
                            {/ 14 14}
                            98})
              "99")
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp `{{anon {[num z] [num y]} : {+ z y}}
                                    {+ 9 14 1}
                                    98})))
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp `{3 4 5})))
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp `{{anon {[num z] [num y]} : {+ z y}}
                                    {+ 9 14}
                                    98
                                    27})))
(check-exn (regexp (regexp-quote "OAZO: Can not divide by 0"))
           (lambda () (top-interp `{{anon {[num z] [num y]} : {+ z y}}
                                    {/ 9 0}
                                    98})))
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp `{{anon {} : {1}} 3 4})))
(check-exn (regexp (regexp-quote "OAZO: TYPE ISSUE BUDDYYY"))
           (lambda () (top-interp '{{{anon {} : 3}} 4 5})))
(check-equal? (top-interp '{arr 4 0})
              "#<array>")

(check-equal? (top-interp '{let
                               {[arr1 : numarray] <- {arr 4 0}}
                             {[arr2 : numarray] <- {arr 4 0}}
                             {arr-eq? arr1 arr2}})
              "false")

(check-equal? (top-interp '{let
                               {[x : numarray] <- {arr 4 99}}
                             {seq
                              {aset x 2 100}
                              {+ {+ {aref x 0} {aref x 2}} {alen x}}}})
              "203")

(check-equal? (get-typed-anon-params '((num x) (num y) (str z)))
              (TypedParams '(x y z) (list (NumT) (NumT) (StrT))))

(check-equal? (is-op? '+) #t)
(check-equal? (is-op? '-) #t)
(check-equal? (is-op? '*) #t)
(check-equal? (is-op? '/) #t)
(check-equal? (is-op? '<=) #t)
(check-equal? (is-op? 'num-eq?) #t)
(check-equal? (is-op? 'str-eq?) #t)
(check-equal? (is-op? 'f) #f)


(check-exn (regexp (regexp-quote "OAZO: Could not find index 100 in store"))
           (lambda () (fetch 100 top-sto)))
(check-equal? (fetch 3 top-sto) (PrimV prim-))

(define test-env : Env (list
                        (Binding 'x 1)
                        (Binding 'y 2)))
(check-equal? (lookup 'x test-env) 1)
(check-equal? (lookup 'y test-env) 2)
(check-exn (regexp (regexp-quote "OAZO: Could not find name 'z in env"))
           (lambda () (lookup 'z test-env)))