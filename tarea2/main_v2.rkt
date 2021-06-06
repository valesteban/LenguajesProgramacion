#lang play
(print-only-errors #t)

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | <string>
         | {if <expr> <expr> <expr>}
         | {fun {<id>*}}  <expr>}
         | {<expr> <expr>*}
         | {local {<def>*} <expr>}
         | {match <expr> <case>+}
<case> ::= {'case <pattern> '=> <expr>}
<pattern> ::= <num>
         | <bool>
         | <string>
         | <id>
         | (<constr-id> <attr-id>*)
<def>  ::= {define <id> <expr>}
         | {datatype <typename> <type-constructor>*}}
<type-constructor> ::= {<id> <member>*}
<constr-id> :: = <id>
<attr-id> :: = <id>
<typename> :: = <id>
<member>   :: = <id>
|#
; expresiones
(deftype Expr
  (num n)
  (bool b)
  (str s)
  (ifc c t f)
  (id s)
  (app fun-expr arg-expr-list)
  (prim-app name args)   ; aplicación de primitivas
  (fun id body)
  (lcal defs body)
  (mtch val cases))


; definiciones
(deftype Def
  (dfine name val-expr) ; define
  (datatype name variants)) ; datatype

; variantes
(deftype Variant
  (variant name params))

; estructuras de datos
(deftype Struct
  (structV name variant values))

; caso en pattern matching
(deftype Case
  (cse pattern body))

; patrón
(deftype Pattern
  (idP id) ; identificador
  (litP l) ; valor literal
  (constrP ctr patterns)) ; constructor y sub-patrones

;; parse :: s-expr -> Expr
(define(parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (str s-expr)]
    [(? symbol?) (id s-expr)]
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun xs b) (fun xs (parse b))]
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local defs body)
     (lcal (map parse-def defs) (parse body))]
    [(list 'match val-expr cases ...) ; note the elipsis to match n elements
     (mtch (parse val-expr) (map parse-case cases))] ; cases is a list
    [(list f args ...) ; same here
     (if (assq f *primitives*)
         (prim-app f (map parse args)) ; args is a list
         (cond
           [(equal? f 'list) ( parse (convertir-a-cons  args)) ] ;aqui chantamente si nos encontramos con un list lo convierte a cons 
           [else  (app (parse f) (map parse args))])
         )]))

;convertir-a-cons: src -> src
;recibe el sintaxis concreta una lista y la ocnvierte a una lista de cons en sintaxis concreta
(define (convertir-a-cons args )
  (if (empty? args)
      (list 'Empty )
      (let ([cosa1 (first args) ])
        (list 'Cons cosa1  (convertir-a-cons (cdr args))))))


; parse-def :: s-expr -> Def
(define(parse-def s-expr)
  (match s-expr
    [(list 'define id val-expr) (dfine id (parse val-expr))]
    [(list 'datatype name variants ...) (datatype name (map parse-variant variants))]))

; parse-variant :: sexpr -> Variant
(define(parse-variant v)
  (match v
    [(list name params ...) (variant name params)]))

; parse-case :: sexpr -> Case
(define(parse-case c)
  (match c
  ;  [(list 'case pattern => body)  (cse (parse-pattern   (convertir-a-cons pattern )) (parse body)) ]))
  ;  [(list 'case pattern => body) (cse (parse-pattern  pattern ) (parse body))])) ;est ee sel del principio
    [(list 'case pattern => body) (cond
                                    [(is?  pattern) (cse (parse-pattern  pattern ) (parse body)) ]
                                    [ (equal? (first pattern) 'list)(cse (parse-pattern  (conv pattern) ) (parse body))   ]
                                    [else   (cse (parse-pattern  pattern ) (parse body))])]))


   ; [(list 'case pattern => body)   pattern ]))

;is? :: src -> #f|#t
;si algo no es una lista retorna  true
(define (is? l)
  (match l
    [(? number?) #t]
    [(? symbol?) #t]
    [(? boolean?) #t]
    [else #f]))


;conv:: src -> src
;recibe el sintaxis concreta una lista y la ocnvierte a una lista de cons en sintaxis concreta
(define (conv p)
  (cond
    [(empty? p)  (list 'Empty )  ]
    [(symbol? p)  p]
    [(let ([val (first p) ])
        (cond
          [ (equal? 'list val) (list 'Cons (conv (second p))(conv (cdr  (cdr p))))     ]
          [ else  (list 'Cons (conv val)  (conv (cdr p))) ])
        )]
    )
  )



;funcion-elegidora:: List[expr] -> List[expr y o valores]
;aqui la funcion elegira al elemento de la lista lo interpretamos o lo cambiamos a exprV
;(define (funcion-elegidora list) ;[list (num 1 ) (prim-ap ...)]  

                                                 


; parse-pattern :: sexpr -> Pattern
(define(parse-pattern p)
  (match p
    [(? symbol?)  (idP p)]
    [(? number?)  (litP (num p))]
    [(? boolean?) (litP (bool p))]
    [(? string?)  (litP (str p))]
    [(list ctr patterns ...) (constrP (first p) (map parse-pattern patterns))]))

;; interp :: Expr Env -> number/boolean/procedure/Struct
(define(interp expr env)
  (match expr
    ; literals
    [(num n) n]
    [(bool b) b]
    [(str s) s]
    ; conditional 
    [(ifc c t f)
     (if (interp c env)
         (interp t env)
         (interp f env))]
    ; identifier
    [(id x)   (if (equal? 'eee x) env   (strict(env-lookup x env)))]  ;(strict(env-lookup x env))]  ; aca va  abuscar el id al ambiente y si se encuentar con una exprV la tiene que evaluar
    ; function (notice the meta interpretation)
    [(fun ids body)
      (λ (arg-vals)
        (def ammmm (cdr arg-vals))
        (def new-arg   (map (λ (a) (interp a ammmm)) (car arg-vals))    )
        (interp body (extend-env ids new-arg env)))]
   
    ; application
    [(app fun-expr arg-expr-list)   ;  arg-expr-list -> (list (num 1) (prim-ap ....))
     (cond
       [ ( id? fun-expr)    ((interp fun-expr env)(cons arg-expr-list env )  )] ;les paso su ambiente y valor ahic adoa uno decide si lo interpreta o no
                             ;(map (λ (a) (interp a env)) arg-expr-list))] ;se busca id en el anbiente  {List? {Cons 1 2}}} -> (list (structV 'List 'Cons '(1 2)))
                                                                                                     ;{T? x}               -> (list (structV 'T 'C '(1)))         
;(app (id 'define) (list (id 'pred) (fun '(n) (mtch (id 'n) (list (cse (constrP 'Zero '()) (app (id 'Zero) '())) (cse (constrP 'Succ (list (idP 'm))) (id 'm)))))))   
       [ (fun? fun-expr) (def (clousureV arg body fenv)  (creacl fun-expr env));(strict (interp fun-expr env)))  (creacl funcion env)
                               (def new-arg-expr-list (guarda-list-expr arg arg-expr-list fenv) ) ; lista sera el valor  o exprV dependiendo de lo q se dijo
                               (def new-list-ids (sacar arg)) ;(list x y z ...)
                               (def new-env  (extend-env new-list-ids
                                                         new-arg-expr-list
                                                         fenv) )
                               (interp body new-env)])] 
    ; primitive application
    [(prim-app prim arg-expr-list)
     (apply (cadr (assq prim *primitives*))
            (map (λ (a) (strict (interp a env))) arg-expr-list))]  ;en caso que una  suma , resta, ... uno de sus arg sean exprV
    ; local definitions
    [(lcal defs body)
     (def new-env (extend-env '() '() env))
     (for-each (λ (d) (interp-def d new-env)) defs)
     (interp body new-env)]
    ; pattern matching


    [(mtch expr cases)
     (def value-matched (interp expr env))
 (cond
       [ (structV? value-matched )
         (cond
           [(empty? (structV-values value-matched))
            (def (cons alist body) (find-first-matching-case value-matched cases))
            (interp body (extend-env (map car alist) (map cdr alist) env))]
           [(Expr? (first (structV-values value-matched)))
            (def new-struct (structV(structV-name value-matched)(structV-variant value-matched)(map (lambda (a) (interp a env )) (structV-values value-matched) )))
            (def (cons alist body) (find-first-matching-case new-struct cases))
            (interp body (extend-env (map car alist) (map cdr alist) env))]
           [else ;(and (> (largo (structV-values value-matched ) )1 ) (structV? (second (structV-values value-matched ))))  ;caso recursivo
            (def (cons alist body) (find-first-matching-case value-matched cases))
            (interp body (extend-env (map car alist) (map cdr alist) env))]
           )]
       [else
        (def (cons alist body) (find-first-matching-case value-matched cases))
          (interp body (extend-env (map car alist) (map cdr alist) env))])])

 
     ;(def new-struct (structV  (structV-name value-matched)  (structV-variant value-matched)  (map (lambda (a) interp a env ) (structV-values value-matched) ) ))
   
  ;   (def (cons alist body) (find-first-matching-case value-matched cases))
  ;   (interp body (extend-env (map car alist) (map cdr alist) env))]
    
)

;sacar :: List[symbol and list] -> List[symbol]
;saca el valor de una list(list x (list 'lazy y)) -> (list x y)
(define (sacar l)
  (if (empty? l)
      '()
      (let ([ val (car l) ])
        (cond
          [(symbol? val ) (cons val (sacar  (cdr l)))]
          [else           (cons (second val) (sacar  (cdr l)))])))
  )

;crea clausura para funciones
(define (creacl funcion env)    ;(fun ids body)
     (clousureV (fun-id funcion )
                (fun-body funcion)
                env))


;guarda-list-expr :: List[arg] List[expr] -> List[expr y exprV]
;funcion encargada de dejar en la lista el valor numerico o la expresion/contrato, es asi como se van a guardar en el ambiente
(define (guarda-list-expr arg arg-expr-list fenv)     ;arg = (list x (lazy y))
  (if (empty? arg)                                    ;arg-expr-list = (list (num 1 ) (prim-app ...))
      '()
      (let ([id (car arg)  ]
            [val(car arg-expr-list) ])
        (cond
          [(symbol? id) (cons (interp val fenv)   (guarda-list-expr (cdr arg ) (cdr arg-expr-list) fenv)  )   ]
          [ else   (cons (exprV val fenv (box #f)) (guarda-list-expr (cdr arg ) (cdr arg-expr-list) fenv)  )    ])
        
  ))) 
; interp-def :: Def Env -> Void
(define(interp-def d env)
  (match d
    [(dfine id val-expr)
    ; (update-env! id (interp val-expr env) env)];
     (update-env! id (interp val-expr env) env)]
    [(datatype name variants)
     ;; extend environment with new definitions corresponding to the datatype
     (interp-datatype name env)
     (for-each (λ (v) (interp-variant name v env)) variants)]))

 

; interp-datatype :: String Env -> Void
(define(interp-datatype name env)
  ; datatype predicate, eg. Nat?
  (update-env! (string->symbol (string-append (symbol->string name) "?"))
               (λ (v) (symbol=? (structV-name (interp (first (car v))  (cdr v))) name))
               ;(λ (v) (interp (first (first v))  (second v)));(first (first v)))
               env))
 
; interp-variant :: String String Env -> Void
(define(interp-variant name var env)
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  (def argumentostipo (variant-params var))
  ;; variant data constructor, eg. Zero, Succ
  (update-env! varname
                (λ (args ) (structV name varname (guarda-list-expr2 argumentostipo (car args)  (cdr args)) )   )
              ; (λ (arg-env) arg-env  )
              ;  (λ (v) v)
               env)
  ;; variant predicate, eg. Zero?, Succ?
  (update-env! (string->symbol (string-append (symbol->string varname) "?"))
              ; (λ (v) (symbol=? (structV-variant (first v)) varname))
              ; (λ (v) v)
              (λ (v) (symbol=? (structV-variant (interp (first (car v))  (cdr v))) varname)    ) ;uno de esos first lo tengo q cambiar a car
              
               env))




(define (guarda-list-expr2 arg arg-expr-list fenv)     ;arg = (list x (lazy y))
  (if (empty? arg-expr-list)                                    ;arg-expr-list = (list (num 1 ) (prim-app ...))
      '()
      (let ([id (car arg)  ]
            [val(car arg-expr-list) ])
        (cond
          [(symbol? id) (cons (interp val fenv)   (guarda-list-expr2  arg  (cdr arg-expr-list) fenv)  )   ]
          [ else   (cons  val  (guarda-list-expr2 (cdr arg ) (cdr arg-expr-list) fenv)  )    ])
        
  )))




  

;;;;; pattern matcher
(define(find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (cse pattern body) cs)
     (let [(r (match-pattern-with-value pattern value))]
       (if (foldl (λ (x y)(and x y)) #t r)
           (cons r body)
           (find-first-matching-case value cs)))]))

(define(match-pattern-with-value pattern value)
  (match/values (values pattern value)
                [((idP i) v) (list (cons i v))]
                [((litP (bool v)) b)
                 (if (equal? v b) (list) (list #f))]
                [((litP (num v)) n)
                 (if (equal? v n) (list) (list #f))]
                [((constrP ctr patterns) (structV _ ctr-name str-values))
                 (if (symbol=? ctr ctr-name)
                     (apply append (map match-pattern-with-value
                                        patterns str-values))
                     (list #f))]
                [(x y) (error "Match failure")]))


#|-----------------------------
Environment abstract data type
empty-env   :: Env
env-lookup  :: Sym Env -> Val
extend-env  :: List[Sym] List[Val] Env -> Env
update-env! :: Sym Val Env -> Void
|#
(deftype Env
  (mtEnv)
  (aEnv bindings rest)) ; bindings is a list of pairs (id . val)

(def empty-env  (mtEnv))

(define(env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (cdr binding)
         (env-lookup id rest))]))

(define (extend-env ids vals env)
  (aEnv (map cons ids vals) ; zip to get list of pairs (id . val)
        env))

;; imperative update of env, adding/overriding the binding for id.
(define(update-env! id val env)
  (set-aEnv-bindings! env (cons (cons id val) (aEnv-bindings env))))

;;;;;;;

;;; primitives
; http://pleiad.cl/teaching/primitivas
(define *primitives*
  `((+       ,(lambda args (apply + args)))
    (-       ,(lambda args (apply - args)))
    (*       ,(lambda args (apply * args)))
    (%       ,(lambda args (apply modulo args)))
    (odd?    ,(lambda args (apply odd? args)))
    (even?   ,(lambda args (apply even? args)))
(/       ,(lambda args (if (equal? (second args) 0)
                               (error "division by zero")
                               (apply / args))))
    (=       ,(lambda args (apply = args)))
    (<       ,(lambda args (apply < args)))
    (<=      ,(lambda args (apply <= args)))
    (>       ,(lambda args (apply > args)))
    (>=      ,(lambda args (apply >= args)))
    (zero?   ,(lambda args (apply zero? args)))
    (not     ,(lambda args (apply not args)))
    (and     ,(lambda args (apply (lambda (x y) (and x y)) args)))
    (or      ,(lambda args (apply (lambda (x y) (or x y)) args)))))





;;;


(define (largo lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))


;pretty-printing :: structV | Val -> string
;funcion que toma unan structira y retorna un string ma sbonito para verlo
(define (pretty-printing l)
  (if (or (number? l) (boolean? l) (symbol? l))
      l
      (let (( name    (structV-name l))
            ( values  (structV-values l))
            ( variant (structV-variant l))
            ( largo-val (largo  (structV-values l) )))
        (if (empty? values)  (format "{~a}" variant) (cond
                                                       [(= largo-val  1)   (format "{~a ~a}" variant (pretty-printing ( first  values)))  ]
                                                       [else (format "{~a ~a ~a}" variant  (pretty-printing (first values )) (pretty-printing ( second  values)))      ])))))





;run :: src -> structV | val | string
;parsea, interpreta y convierte sructuras a valores string mas amigables
(define(run prog [flag ""])
  (let ([l `{local {{datatype List 
                  {Empty} 
                  {Cons n1 rec}}
                {define length {fun {n} 
                               {match n
                                 {case {Empty} => 0 }
                                 {case {Cons m1 m2 } => {+ 1 { length m2 } }}}}}} ,prog }])
    
    (let ([val (interp (parse l) empty-env ) ])
      (cond
        [(equal? "ppwu" flag) (if (number? val )  val (pretty-printing val ))]
        [(equal? "pp" flag)
         (cond
           [(number? val )  val]
           [(equal? (structV-name val)'List) (listasbonitas (prr val ))]
           [else (format "{ ~a ~a } "(structV-variant val)(tt2 (structV-values val) (structV-variant val) ))])]
        [else  val ])))
 )



;tt :: List|val -> String
;funcion que transforma una struct en string bonito
(define (tt2 b nombre)
  (if (empty? b)
      ""
      (let ([primval (first b) ])
        (cond
          [(list? primval )  (format "~a ~a"(agrega primval nombre) (tt (cdr b))) ]
          [else (format "~a ~a"primval (tt (cdr b))) ]))
      )
  )
;en caso de ser recursiva agregar nombre variant
(define (agrega b nombre)
  (format "{ ~a ~a }" nombre (tt2 b) )) ;(1 (2 3))

;prr :: structV -> cons 
;funcion que transforma nustras estructuras de nuestro lenguaje a un cons de racket
(define (prr l)
  (if (or (number? l) (boolean? l) (symbol? l))
      l
      (let (( name    (structV-name l))
            ( values  (structV-values l))
            ( variant (structV-variant l))
            ( largo-val (largo  (structV-values l) )))
        (if (empty? values)  '()  (cond
                                           [(= largo-val  1)   (cons  (prr ( first  values)))  ]
                                           [else (cons (prr (first values )) (prr ( second  values)) )   ])))))

;listasbonitas :: cons -> String
;funcin que transforma de cons de racket a una lista bonita con la ayuda de la funcino rr, esta solo se encarga de poner un list si es necesario
(define (listasbonitas b)
  (format "{list ~a}" (tt b))) ;(1 (2 3))


;tt :: List|val -> String
;funcion que recibe una lista o valor y si es un valaor lo retorna pero sii es una lista llama a la anterir para ver que hacer con ella
(define (tt b)
  (if (empty? b)
      ""
      (let ([primval (first b) ])
        (cond
          [(list? primval )  (format "~a ~a"(listasbonitas primval) (tt (cdr b))) ]
          [else (format "~a ~a"primval (tt (cdr b))) ]))
      )
  )


(deftype Exprg
  [clousureV arg body env]
  [exprV expr env cache]) ;"promesa" estos expresiones la guardara el ambiente

;strict :: val -> val
;funcion que se encarga de cumplir las promesaa, es decir aqui se evaluan finalmente las expresiones a usar
(define (strict val)
  (match val
    [(exprV expr env cache )
     (if (unbox cache)
         (begin (unbox cache))
         (let ([ inval (strict (interp expr env))])
           (set-box! cache inval)
           inval))]
    [else val]))





(run '{local {{datatype T {C a {lazy b}}}
                {define x {C  0 {+ 1 2}}}}
               x} "pp")





; (run '{local {{datatype Nat                                                    ;ejemplo enunciado
;                  {Zero} 
;                  {Succ n}}
;                {define pred {fun {n} 
;                               {match n
;                                 {case {Zero} => {Zero}}
;                                 {case {Succ m} => m}}}}}
;          {pred {Succ {Succ {Succ {Zero}}}}}} "ppwu")




; (run '{match {list 2 {list 4 5} 6}           ;ejemplo enunciado
;          {case {list a {list b c} d} => c}}) ;5)




;(run '{match 2
;                {case 1 => 2}
;                {case 2 => 3}})
;        

;(run '{local {{datatype T 
;                  {C {lazy a}}}
;                {define x {C {/ 1 1}}}}
;          {match x
;            {case {C a} => a}}})
;"/: division by zero"
; (run '{length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}) 


