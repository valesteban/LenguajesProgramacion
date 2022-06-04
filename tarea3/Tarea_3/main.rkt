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
         ------PARAOBJETOS------------
         | (object [: <expr>] <member> ...)
         | this
         | (set <id> <expr>)
         | (get <id>)
         | (send <expr> <id> <expr> ...)
         | (shallow-copy <expr>)
         | (deep-copy <expr>)
         -----FUNCIONES LAMBDAS-------
         | (fun (<id>*) <expr>)
         | (<expr> <expr>*)


<def>    ::= (define <id> <expr>)


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
  (this yo)              
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
    [(? symbol?)
     (if (equal? s-expr 'this)(this 'yo)(id s-expr))]
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
       (object expresion (lista-field lista-mem)(lista-method lista-mem))) ]
    ;caso con expresion
    [(list 'object  list-valores-metodos ...)
     (let ([   lista-mem (map parse-member list-valores-metodos )  ])
        (object 'NO (lista-field lista-mem) (lista-method lista-mem) ))]
    
    [(list 'get id) (get id)]
    [(list 'set id val) (set id (parse val))]
    
    [(list 'send obj met lista-v ...)
     (send (if (equal? 'this (parse obj) )(parse obj) (parse obj)) met (map (lambda(arg)(parse arg) )lista-v))]

 
    [(list 'shallow-copy expr) (shallow-copy (parse expr))]
    [(list 'deep-copy expr) (deep-copy (parse expr))]
    ;------LAMBDA/FUN------------------------------------
    [(list 'fun listarg cuerpo);method nombre lista-valores cuerpo  ))
     (object 'NO empty (list (method 'fun listarg (parse cuerpo))))]
    [(list id val ...)
     (send (parse id ) 'fun (map (lambda(arg)(parse arg) )val))]
 
    
    ))

;lista-field:: List[Member] -> List[field]
;funcion que recibe una lista de fields y metodos y devuelve una lista con solo los fieds
;(lista-method (list (field 'x (num 1)) (field 'y (num 2)) (field 'z (num 55)) (method 'sum '(z) (num 1) ) (method 'set-x '(val)  (num 5))) )
(define (lista-field lista-mem )
  (if (empty? lista-mem)
      empty
      (let ([mem (car lista-mem ) ])
        (if (field? mem )
            (cond
              [(> (length lista-mem) 1)  (cons mem (lista-field (cdr lista-mem))) ]
              [else  (cons mem empty ) ])
     
        empty))))



;lista-method:: List[Member] -> List[method]
;funcion que recibe una lista de fields y metodos y devuelve una lista con solo metodos    
;(lista-method (list (field 'x (num 1)) (field 'y (num 2)) (field 'z (num 55))
;(method 'sum '(z) (num 1) ) (method 'set-x '(val)  (num 5))) )
(define (lista-method lista-mem )
  (if (empty? lista-mem)
      empty
      (let ([mem (car lista-mem ) ])
        [cond
          ((method? mem )   (cons  mem (lista-method (cdr lista-mem))  ))
          ( else (lista-method (cdr lista-mem ))  )])))
        


;parse-member:: s-expr -> Member
;funcion encargada una parte de codigo y devolver un Member
(define (parse-member codigo)
  (match codigo
    [(list 'field id val) (field id (parse val))]
    [(list 'method nombre list-id cuerpo)(method nombre list-id (parse cuerpo))]))

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
    [(id x) (if (equal? 'aaa x) env  (env-lookup x env))]
    [(seqn expr1 expr2) (begin
                          (interp expr1 env)
                          (interp expr2 env))]
    [(lcal defs body);<-----------------------------------LOCAL--------------------------------------------
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (def (cons id val) (interp-def x new-env))
                   (extend-frame-env! id val  new-env)
                   #t) defs)
       (verificar body) ;verifica que no se usen this, get y set
       (interp  body new-env))

       ]
     [(object expre lista-field lista-metodos);<--------------OBJECT-------------------      
      (def amb (multi-extend-env '() '() env)) ;creamos un nuevo ambiente para nuestro objeto   
      (if (equal? 'NO expre) amb (extend-frame-env! 'papo (interp expre env)  amb))
      (def ambienteobj  (crear-ambiente-objeto expre lista-field lista-metodos amb ));
      (objectV expre (cambia-box-fields lista-field env) lista-metodos ambienteobj)

      ]  
 
    [(send nom nombremet lista-val-num);<----------------------SEND----------------------
     ;buscamos al objeto
     (def objj;(objectV expresion fieldss metodoss suamb)
       (if (box? (interp nom env)) (unbox (interp nom env)) (interp nom env)))
     (def objeto-met (busca-met-par objj nombremet));(cons objeto[padreohijo]  metodo)   
     ;El metodo a ocupar
     (def (method nombre listavalores-sinnum cuerpo) (first (cdr  objeto-met)))

     ;interpreto los valorres de la lista de argumentos
     (def listayainterpreta (map (lambda(l ) (interp l env)) lista-val-num))

     ;dependiendo de si es un get o un set [se ocupa o no el ambiente del pade od el hijo]
      (def new-env (multi-extend-env '() '()
                                     (if  (or (get? cuerpo)(set? cuerpo))   
                                     (objectV-amb (car objeto-met)) ;ocupamos el ambiente de quien sea el metodo
                                     (objectV-amb objj);ocupamos el ambiente del q se llamo
                                     )))
     ;agregamos los arg de a el ambiente
     (for-each (λ(num ind)
                  (extend-frame-env! ind num  new-env) 
                 #t) listayainterpreta listavalores-sinnum)

     (interp cuerpo new-env)
      ]
    [(get nombrefield) ;<-------------------------------------GET--------- -------------
     ;buscamos ese field en el ambiente del objeto que solo se tiene a si mismo
     (def (field nombre boxito)(buscar-field-suyo nombrefield env)) ;(field 'y (box (numV 5)))

     (interp (unbox boxito) env )]
    [(set indi val) ; <----------------------------------------SET----------------------
    (def valnum (make-expr (if (Expr? val) (interp val env) val )))
    (set-box! (field-valor (buscar-field-suyo indi env)) valnum ) ;cambia el valor
    ]
    [(this 'yo) 
     (def miobjetolindo(env-lookup 'yo env))
     miobjetolindo]

    [(shallow-copy obj); <------------------------------SHALLOW-COPY---------------------------
     ;buscamos al objeto que se va a copiar
     (def fff
       (if (box? (interp obj env))
           (unbox (interp obj env) )
           (interp obj env)))

     ;buscar a su yo en el ambiente porque ese es el que realmiente quiero copiar
     ;(printf "~a"(env-lookup'yo suamb));(interp 'yo suamb))
    (def (objectV expresion fields metodos suamb) (if (box? (env-lookup'yo (objectV-amb fff)))
                                                          (unbox (env-lookup'yo (objectV-amb fff)))
                                                          (env-lookup'yo (objectV-amb fff))))


     ;tengo que crear el ambiente de este nuevo objeto y guardarse a si mismo
     (def amb (multi-extend-env '() '() (objectV-amb fff)))
     (if (equal? 'NO expresion) amb (extend-frame-env! 'papo (env-lookup 'papo suamb)  amb))
    ; (printf "~a" (env-lookup 'papo suamb))

    
     (def ambienteobj  (crear-ambiente-objeto  expresion (crear-copia-fields  fields) metodos amb ))

 

     
     (objectV expresion  (crear-copia-fields fields) metodos ambienteobj) ;(crear-copia-metodos   metodos)

     

    ]
    [(deep-copy obj) ;<----------------------------DEEP-COPY------------------------------------

     (def fff
       (if (box? (interp obj env))
           (unbox (interp obj env) )
           (interp obj env)))

     ;buscar a su yo en el ambiente porque ese es el que realmiente quiero copiar
     ;(printf "~a"(env-lookup'yo suamb));(interp 'yo suamb))
    (def (objectV expresion fields metodos suamb) (if (box? (env-lookup'yo (objectV-amb fff)))
                                                          (unbox (env-lookup'yo (objectV-amb fff)))
                                                          (env-lookup'yo (objectV-amb fff))))


     ;tengo que crear el ambiente de este nuevo objeto y guardarse a si mismo
     (def (objectV expresionp fieldsp metodosp suambp) (env-lookup 'papo suamb))
     
     (def amb (multi-extend-env '() '() (objectV-amb fff)))
     (if (equal? 'NO expresion) amb (extend-frame-env! 'papo (objectV expresionp
                                                                          (crear-copia-fields fieldsp)
                                                                          metodosp suambp)   amb))
    ; (printf "~a" (env-lookup 'papo suamb))

    
     (def ambienteobj  (crear-ambiente-objeto  expresion (crear-copia-fields  fields) metodos amb ))

 

     
     (objectV expresion  (crear-copia-fields fields) metodos ambienteobj) ;(crear-copia-metodos   metodos)

     



     
     ] 
      ))






;;verificar:: Expr -> error|Expr
;funcion que verifica que no hallan un this, get o set fuera de un objeto
(define (verificar body )
  (cond
    [(get? body) (error "get used outside of an object")]
    [(set? body) (error "set used outside of an object")]
    [(this? body) (error "this used outside of an object")]
    [(seqn? body) (verificar (seqn-expr1 body)) (verificar (seqn-expr2 body)) ]
    [else body]
  ))
       
;crear-ambiente-objeto:: expr List[field] env -> objectV
;funcion que nos ayudara a crear un objecto que en su ambiente se tenga a si mismo
(define (crear-ambiente-objeto expre lista-field lista-metodos env )
  (def box-val (box 'undefine))
  (def nuevoamb (multi-extend-env '() '() env))
  (extend-frame-env! 'yo box-val  nuevoamb)
  (def obj (objectV expre (cambia-box-fields lista-field env) lista-metodos nuevoamb))
  (set-box! box-val obj)
  nuevoamb)

;crear-copia:: List[fields] -> List[fields]
;funcion que crea unna copia de la lista de fields, esto lo hice porque si  no se quedaba como
;"referenciado al otro" y entonces si el otro cambiaba e este tambien
(define (crear-copia-fields list)
(if (or (null? list)(empty? list)) 
  '() 
  (if (list? list)
      (let ([primero (car list) ]) ;(field 'count (box (num 0)))
      (cons (field (field-nombre primero)(if (box? (field-valor primero) )
                                             (box (make-expr(make-val(open-val(unbox (field-valor primero))))))
                                             (make-expr(open-expr (field-valor primero)))))



            (crear-copia-fields (cdr list))))
      list)))
 
;crear-copia-metodos:: List[methods] -> List[methods]
;funcion que crea unna copia de la lista de metodos, esto lo hice porque si  no se quedaba como
;"referenciado al otro" y entonces si el otro cambiaba e este tambien
(define (crear-copia-metodos list)
(if (or (null? list)(empty? list)) 
  '() 
  (if (list? list)
      (let ([primero (car list) ]) ;(field 'count (box (num 0)))
      (cons (method (method-nombre primero) (method-lista-valores (copia-normal primero))(method-cuerpo primero))
            (crear-copia-metodos (cdr list))))
      list)))

;copiar-papo:: object env -> objectV
;funcion que hacer una copia al objeto padre
(define (copiar-papo papo env)
  (cond
    [(object? papo)
     (def (object expresion fields metodos ) papo)
     (objectV  expresion (crear-copia-fields  fields) metodos env )      ]))
 
;buscar-field-suyo:: sym env -> val
;funcion que se mete en su ambiente encuentra a yo y luego busca   el field correspsondient
(define (buscar-field-suyo nombrefield env)
  (def (objectV expr listafield listametodos enb)(unbox (env-lookup 'yo env)))
  (cond
    [(not (empty? listafield))
     (if (empty? (filter (lambda(n)(equal? (field-nombre n) nombrefield ) ) listafield))
     (error "field not found")
     (car (filter (lambda(n)(equal? (field-nombre n) nombrefield ) ) listafield)))   ]
    [else (error "field not found") ]
   ; [(equal?' NO expr) (error "field not found")]
   ; [else (buscar-field-suyo-padre  nombrefield env )] ;buscamso en el padre
     ) 
  )

;copia-normal:: List[sym] -> List[sym}
;funcion que me va a copiar la lista de argumentos de una funcion definida en en un metodo
(define (copia-normal list)
(if (null? list) 
  '() 
  (if (list? list) 
      (cons (copia-normal (car list)) (copia-normal (cdr list)))
      list)))



;buscar-field-padre:: sym env -> val
;funcion que se mete en su ambiente encuentra al padre y luego busca   el field correspsondient, si tamapoco vlo encuentra
;va a buscar al padre del padre
(define (buscar-field-suyo-padre  nombrefield env )
  (def (objectV expr listafield listametodos enb)(if (box? (env-lookup 'papo env)) (unbox (env-lookup 'papo env)) (env-lookup 'papo env)))
  (cond
    [(not (empty? listafield))  (car (filter (lambda(n)(equal? (field-nombre n) nombrefield ) ) listafield))   ]
    [(equal?' NO expr) (error "field not found")]
    [else (buscar-field-suyo-padre  nombrefield enb )] ;buscamso en el padre
     ) 
  )


;busca-met-par:: objectV -> cons objectV List[method]
;funcion que busca si un metodo en un objeto y en caso que esta devuelve un par que contiene 
;al objeto y su metodo
(define (busca-met-par objeto nombremet)
  (let ([met   (filter
                    (lambda(n)(equal? (method-nombre n) nombremet ))
                    (if  (box? object)
                         (objectV-metodos (unbox objeto))
                         (objectV-metodos objeto))  )])
    (cond
      [(empty? met ) (if (equal? (objectV-exp objeto) 'NO)
                        (error "method not found")
                         (busca-met-par (if (box? (env-lookup 'papo (objectV-amb objeto)))
                                            (unbox (env-lookup 'papo (objectV-amb objeto)))
                                           (env-lookup 'papo (objectV-amb objeto))) nombremet) )];significa que no se encontró el metodo en el padre, si que si esq tiene buscamos en el padre
      [else (cons objeto  met )] ;si se encontro wntonces se devuelve
      ))
  )

  


;cambia-box-fields:: fields -> List
;funcion que a el volor de un field lo convierte en una box 
(define (cambia-box-fields fields env)
  (if (empty? fields )
      empty
      (let ([fielddd (car fields)])
        (let ([ valor  (field-valor fielddd ) ];no lo vamos a interpretar porque yo quise que fuese evaluacion perezosa
              [ nombre (field-nombre fielddd) ])
          (cons (field nombre (if (box? valor) valor (box valor)) )   (cambia-box-fields (cdr fields) env)   )
    ))))

;obtener-nombre:: Expr -> sym 
;funcion que obtiene el nombre del id , en el fondo esto le va a dar el nombre del objeto que se busca
;por lo talto se le puede pasar un id o un this para referirse a un objeto
(define (obtener-nombre loqsea )
  (match loqsea
    [(id d) d]
    [(this yos) yos ]
    [else loqsea]
    ))
    


;buscar-met:: List[method] sym -> method    
;funcion que busca un metodo dentro de un lista de metodos
(define (buscar-met listametodos nombremetodo)
  (if (empty? listametodos)
      ;(error "method not found")
      (method 'nose empty empty)
      (let ([ metodito (car listametodos) ])
        (if (equal? nombremetodo (method-nombre metodito))
            metodito ;si lo encontro ---> lo retorna
           (buscar-met (cdr listametodos) nombremetodo)    ;si no lo encuentra ---> sigue buscando
         )
        )
      )) 


     

;;make-expr :: Val  -> expr
; funcion que transforma uno de nuestro valores a una expresion
(define (make-expr  s)
  (match s
    [(numV v) (num v) ]
    [(boolV b) (bool b)]
    [(? number?) (num s)]
    [(? boolean?) (bool s)]
    [_ s]
    ))

;;open-expr:: Expr -> scheme Value
;funcion qu transforma una expresion en un valor de scheme
(define (open-expr e)
  (match e
    [(num n) n]
    [(id v) v]
    [_ e]))

;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    [(num n) n]
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
    ;[(my-def id body) (cons id (interp body env))]))
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
    [(? objectV?) (objeto-bonito val)]
    [(box val) (objeto-bonito val)]
    [x x]))

;; a simple monotone counter
(define counter '(object
                  (field count 0)
                  (method incr () (set count (+ 1 (get count))))
                  (method get () (get count))))

;; sequence of operations over 2 counters
(define (incrs-multiply x y)
  `(seqn
    (send ,y incr)
    (seqn
     (send ,x incr)
     (seqn
      (send ,x incr)
      (* (send ,x get) (send ,y get))
      ))))

;objeto-bonito:: object|box -> string
;funcion que recibe un objeto o una box con un objeto y devuelve un string para ver el el objeto ma sbonito
(define (objeto-bonito obj)
  (def (objectV exp fields metodos amb) (if (box? obj) (unbox obj) obj) )
  (format "(object padre: ~a  fields: ~a metodos: ~a noquiero ver elambiente" exp  fields metodos))

;-------------------------------------------------------------------


