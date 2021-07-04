 #lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA OBJETOS
<expr>  ::= ... (todo lo anterior)
         | (object [: <expr>] <member> ...)
         | this
         | (set <id> <expr>)
         | (get <id>)
         | (send <expr> <id> <expr> ...)
         | (shallow-copy <expr>)
         | (deep-copy <expr>)


|#

(deftype Expr
  (num n)
  (bool b)
  (id s)
  (binop f l r)
  (unop f s)
  (my-if c tb fb)
  (seqn expr1 expr2)
  (lcal defs body)
  ;DESDE AQUÍ EMPIEZA LO DE OBJETOS
  (object exp list-fields list-methods)            ;  creamos el objeto
  (this)              
  (set id val)                ;  cambiar el valor de una varibale
  (get id)                    ;  acceder al valor de una de sus variables
  (send objeto metodo lista-valores)  ;  permite invocar a un método de un objeto dado, con 0 o más argumentos. (send o set-x (+ 1 3))
  (shallow-copy expr)
  (deep-copy expresion)
  )

#|
<member> ::=
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)
|#
;; Member
(deftype Member
  (field nombre valor)                        ;valor sera una box para d eesa forma manejar la mutabilidad de estos
  (method nombre lista-valores cuerpo  ))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (objectV exp fields metodos amb))

(deftype Def
  (my-def id expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type

empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env))

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest) (hash-ref hash x (λ () (env-lookup x rest)))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-immutable-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))
 
#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (hash id val) env)]
    [(aEnv h rEnv) (def hupd (hash-set h id val))
                   (set-aEnv-hash! env hupd)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    ;------NUEVOS------------------------------------
    ;caso sin expresión
    [(list 'object ': expr list-valores-metodos ...)
     (let ([ expresion (parse expr) ]
           [ lista-mem (map parse-member list-valores-metodos ) ])
       (object expresion (lista-field lista-mem) (lista-method lista-mem))) ]
    ;caso con expresion
    [(list 'object  list-valores-metodos ...)
     (let ([   lista-mem (map parse-member list-valores-metodos )  ])
        (object 'NO (lista-field lista-mem) (lista-method lista-mem) ))]
    
    [(list 'get id) (get id)]
    [(list 'set id val) (set id val)]
    
    [(list 'send obj met lista-v ...) (send obj met lista-v)]

 
    [(list 'shallow-copy expr) (shallow-copy expr)]
    [(list 'deep-copy expr) (deep-copy expr)]
    
    ))

;lista-field:: List[Member] -> List[field]
;funcion que recibe una lista de fields y metodos y devuelve una lista con solo los fieds
;(lista-method (list (field 'x (num 1)) (field 'y (num 2)) (field 'z (num 55)) (method 'sum '(z) (num 1) ) (method 'set-x '(val)  (num 5))) )
(define (lista-field lista-mem )
  (let ([mem (car lista-mem ) ])
    (if (field? mem )
        (cond
          [(> (length lista-mem) 1)  (cons mem (lista-field (cdr lista-mem))) ]
          [else  (cons mem empty ) ])
        
        empty)))



;lista-method:: List[Member] -> List[method]
;funcion que recibe una lista de fields y metodos y devuelve una lista con solo metodos    
;(lista-method (list (field 'x (num 1)) (field 'y (num 2)) (field 'z (num 55)) (method 'sum '(z) (num 1) ) (method 'set-x '(val)  (num 5))) )
(define (lista-method lista-mem )
  (if (empty? lista-mem)
      empty
      (let ([mem (car lista-mem ) ])
        [cond
          ((method? mem )   (cons mem (lista-method (cdr lista-mem))  ))
          ( else (lista-method (cdr lista-mem ))  )])))
        


;parse-member:: s-expr -> Member
;funcion encargada una parte de codigo y devolver un Member
(define (parse-member codigo)
  (match codigo
    [(list 'field id val) (field id (parse val))]
    [(list 'method nombre list-id cuerpo)(method nombre list-id (parse cuerpo))]))

;(parse-member '{field s 3})
;(parse-member '{method sum (z) (+ 1 2)})

 

;parse-method:: sym list[sym] s-expr -> Member
;(define (parse-method nombre list-id cuerpo)
 ; (method nombre ))



;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(binop f l r)
      (make-val (f (open-val (interp l env))
                               (open-val (interp r env))))]
   
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id x)  (if (equal? 'aaa x) env  (env-lookup x env))]
    [(seqn expr1 expr2) (begin
                          (interp expr1 env)
                          (interp expr2 env))]
    [(lcal defs body)
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (def (cons id val) (interp-def x new-env))
                   (extend-frame-env! id   val  new-env)
                   #t) defs)
       ;------>   ver el ambiente   ----->    new-env)]
       (interp body new-env))]
       
       
    
     [(object expre lista-field lista-metodos); <------------------------------------------AQUI SE CREA EL OBJECTO
      ;se evalua primero expr (si esque es algo)
      (if (equal? 'NO expre) (void) (interp  expr   env) )
      ;(def ambienteobj  (crear-ambiente-objeto expre lista-field lista-metodos empty-env )); (multi-extend-env '() '() empty-env)) ;creo un ambiente vacio para mi objeto
      ;(objectV expre (cambia-box-fields lista-field env) lista-metodos ambienteobj)] ;creamos el objeto
      (objectV expre lista-field lista-metodos empty-env)]
  
      
 
    [(send nom nombremet lista-val-num)  ;<-----------------------------------------------OCUPAR METODO DE UN OBJETO
     ;buscamos a el objeto en nuestro ambiente
     (def (objectV expresion fieldss metodoss suamb)(env-lookup nom env) )   ;(objectV )    
     ;buscamos el metodo dentro del objeto
     (def (method nombre listavalores-sinnum cuerpo)(buscar-met metodoss nombremet))  ;  (method sum () (+ 1 2))
     ;agregamos los arg de a el ambiente  y a si mismo
     ;(def new-env (multi-extend-env '() '() suamb) )
     ;(extend-frame-env! 'yo (objectV expresion fieldss metodoss suamb)  new-env) ; en su amabiente va a tener como se llama asi con ese valor ava  abuscarse a el ambiente global
     ;(for-each (λ(num ind)
     ;             (extend-frame-env! ind num  new-env) 
     ;            #t) lista-val-num listavalores-sinnum)
     ;--> mostrar el ambiente new-env --> new-env
     ;--> pregunto al hash un valor -->(hash-ref (aEnv-hash new-env) 'a )
     (for-each (λ(num ind)
                  (extend-frame-env! ind num  env) 
                 #t) lista-val-num listavalores-sinnum)
     
      ;env
cuerpo
     ;(env-lookup x env)
     ;(cond
     ;  [(get? cuerpo)
     ;   (def (field id val ) (car (filter    (lambda(n)(equal? (field-nombre n) (get-id cuerpo ) ) )  fieldss) ))
     ;   (interp val env)]
     ;  [(set? cuerpo) 'ppp]

     ;  [else (interp cuerpo env )]
        
]
 
 

     

    ; (interp cuerpo env)
    ;(interp cuerpo new-env )

     
    [(get nombrefield )
     ;------VERSION DONDE BUSCA EL VALOR EN EL AMBIENTE
     ;buscamos ese field en el ambiente del objeto que solo se tiene a si mismo
     ;(def (field nombre boxito)(buscar-field-suyo nombrefield env)) ;(field 'y (box (numV 5)))
     ;(unbox boxito)]
     ;------VERSION DOND EBUSCA EL FIELD EN SI MISMO NOMAS
     ;buscamos el objeto en el ambiente general
     (def (objectV expresion fieldss metodoss suamb)(env-lookup 'yo env) )
     (def (field ind val)(buscar-field-suyo nombrefield env)) ;  --> (field 'y (num 2))
     (num-n val)  ]
    [(set indi val)
     (def (field ind val)(buscar-field-suyo indi env)) ; ---> (field ý (num 2))
     (set! val (interp val env))]
    ;(def valnum (env-lookup val env))
    ;(set-box! (field-valor (buscar-field-suyo indi env)) valnum ) ;cambia el valor 
    
    ))
     


    ; (aEnv-hash env)]

;crear-ambiente-objeto:: expr List[field] env -> objectV
;funcion que nos ayudara a crear un objecto que en su ambiente se tenga a si mismo
(define (crear-ambiente-objeto expre lista-field lista-metodos env )
  (def box-val (box 'undefine))
  (def nuevoamb (extend-frame-env! 'yo box-val  env))
  (def obj (objectV expre (cambia-box-fields lista-field env) lista-metodos nuevoamb))
  (set-box! box-val obj)
  nuevoamb)

;buscar-field-suyo:: sym env -> val
;funcion que se mete en su ambiente encuentra a yo y luego busca   el field correspsondient
(define (buscar-field-suyo nombrefield env)
  (def (objectV expr listafield listametodos enb)(env-lookup 'yo env))
  (car (filter (lambda(n)(equal? (field-nombre n) nombrefield ) ) listafield)))

;dame-el-field:: sym List[field] -> field
;funcion que buscar un field por nombre y lo devuelve
;(define (dame-el-field nombre listaf)
;  (if empty?)

;cambia-box-fields:: fields -> List
;funcion que a el volor de un field lo convierte en una box 
(define (cambia-box-fields fields env)
  (if (empty? fields )
      empty
      (let ([fielddd (car fields)])
        (let ([ valor (interp  (field-valor fielddd ) env) ]
              [ nombre (field-nombre fielddd) ])
          (cons (field nombre (box valor) )   (cambia-box-fields (cdr fields) env)   )
    ))))
    


;buscar-met:: List[method] sym -> method    
;funcion que busca un metodo dentro de un lista de metodos
(define (buscar-met listametodos nombremetodo)
  (if (empty? listametodos)
      (error "method not found")
      (let ([ metodito (car listametodos) ])
        (if (equal? nombremetodo (method-nombre metodito))
            metodito ;si lo encontro ---> lo retorna
           (buscar-met (cdr listametodos) nombremetodo)    ;si no lo encuentra ---> sigue buscando
         )
        )
      )) 
    
 
     



;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    [ else v]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))


;-----------------------------------------------------------
 (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (a) (+ 100 a))
                          (method get-y () (get y))))]
           (send o sum 55 )))
