#lang play

(print-only-errors #t)
(define w1 '{with {{x 5}{y 7} x}})
(define w2 '{with {{x : Num 5} {y : Num 7} x}})

;largp: List[] -> num
;funcion que ve el largo de una lista
(define (largo lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))

;DEFINICION DE TIPOS
(deftype Type
  [Num]
  [Bool]
  [Any])

;DEFINICIÓN DE ARGUMETOS
(deftype Arg
  (arg0 id type contract))  ;le puse ese nombre porque ya en el codigo hartas cosas teneian nombre argu


#|                                            
<expr>   ::= <num>                             
           | <id>                              
           | <bool>                            
           | {<unop> <expr>}                 
           | {<binop> <expr> <expr>}          
           | {if <expr> <expr> <expr>}          
           | {with {{<id> <expr>}*} <expr>}    
           | {<id> <expr>*}                     
|#

(deftype Expr
  [num n]                       
  [id x]                       
  [bool l]                     
  [unop op e]                 
  [binop op l r]               
  [iff c t f]                  
  [with list-id body]                
  [app fname list-id])          



;<binop>  ::= + | - | * | / | && | = | < | ...
;PARSE DE BINOPS
(define (parse-binop bin)
  (match bin
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['&& (lambda (x y) (and x y))]   
    ['= =]  
    ['< <]
    ['> >]
    )
  )
;DEFINIMOS BINOPS 
(define binops (list '+ '- '* '/ '&& '= '< '>))

;is-binop?: op -> #t/#f
;typechecker de los tipos de binop, cheque que el que se dio, exista
(define (is-binop? x) (member x binops))

;pares-unop: src -> binop
;parsea los unop, los posible operadores
(define (parse-unop bin)
  (match bin
    ['add1 add1]
    ['sub1 sub1]
    [! not] 
    )
  )
;DEFINICION DE UNOPS
(define unops (list 'add1 'sub1 '!))         


;is-unop?: op -> #t/#f
;typechecker de los tipos de unop, chequea que el que se dio, exista
(define (is-unop? x) (member x unops))        

;DEFINICIÓN  DE FUNCIONES
;<fundef> ::= {define {<id> <id>*} <expr>}      
(deftype FunDef
  (fundef name list-param type-result body))

;lookup-fundef :: Sym List[FunDef] -> FunDef (o error)
;Busca una funcion por el nombre de esta en una lista y la devuelve
(define (lookup-fundef f fundefs)
  (match fundefs
    ['() (error "undefined function:" f)]
    [(cons fd fds) (if (equal? f (fundef-name fd))
                       fd
                       (lookup-fundef f fds))]))

;ponetipo: literal -> type
;funcion que pone el correspondiente tipo  dependiendo si tiene 'Num, 'Any o 'Bool
(define (ponetipo str)
  (cond
    [(equal? str 'Num ) (Num)]
    [(equal? str 'Bool) (Bool)]
    )
  )


;parse-la: List[}/src -> Pair 
;funcion que ayuda en el parseo de las funciones,especificamente con la lista de argumentos de esta
(define (parse-la a)  ;(list x y z)
                        ;(list [x : Num] [y : Num ][z : num])
                        ;(list [x : Num @ contr][z : Num @ contr][z : Num @ contr])
  (cond
    [(list? a ) (if (= (largo a) 5 )  (arg0 (first a) (ponetipo(third a)) (fifth a))      (arg0 (first a) (ponetipo(third a)) 'NOCONTRACT))]
    [else  (arg0 a (Any) 'NOCONTRACT)] ))

;funcion que toma la lista de argumentos de una defiicion de funcion y la convierte en una lista de argumentos,
;t corresponde al valor de salida de la funcion
(define (transf-arg lista)
  (let ((largo (largo lista)))
    (if (list? lista)
        (cond               ; x : Num @ contract
          [(> largo 3) (arg0 (car lista) (ponetipo(third lista)) (fifth lista ))  ]
          [(= largo 3) (if (equal? (second lista)'@)  (arg0 (car lista)(Any) (third lista)) (arg0 (car lista) (ponetipo(third lista)) 'NOCONTRACT )) ])
        (arg0 lista (Any) 'NOCONTRACT  )    
      )
         ))
;parse-id: List[] -> List[] 
;funcion que ayuda en el parseo general de la expresio, especificamente con la lista de id,val del with
(define (parse-id lista )
  (let (( largo (largo lista)))
    (cond
      [(if ( > largo  2) (list (car lista) (ponetipo(third lista)) (parse-expr (fourth lista )))    (list (car lista) (Any) (parse-expr (second lista)))  ) ] ; es de la forma nueva
    )
  ))

;pase-expr: src -> Expr
;parsea lal expresion, convierte  la sintaxis concreta en abstracta
(define (parse-expr src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(? boolean?)(bool src)]                          
    [(list (? is-binop? op) l r) (binop (parse-binop op) (parse-expr l) (parse-expr r))]
    [(list (? is-unop? op) e) (unop (parse-unop op) (parse-expr e))]
    [(list 'if c t f) (iff (parse-expr c) (parse-expr t) (parse-expr f))]
    [(list 'with (? list? id ) b)
     ; {with {{x : Num 5} {y : Num 7}} x} ---> (with (list (list x Num (num 5)) (list y Num (num 7))) (id x))
     ; {with {{x 5} {y 7}} x}       ---------> (with (list (list x Any (num 5)) (list y Any (num 7))) (id x))
     (with (map parse-id id) (parse-expr b))]
    
    [(list 'define list1 exp )   ; CASO DONDE NO SALE EL VALOR DE SALIDA   '{define {gt42 x}  {> x 42}})
     (fundef (first list1) (map transf-arg (cdr list1)) (Any) (parse-expr exp) )]
    
    [(list 'define list1 : type  exp ) ; CASO DONDE SI SALE EL VALOR DE SALIDA
     (fundef (first list1) (map transf-arg (cdr list1)) (ponetipo type) (parse-expr exp) )]
    [(list fname x ...) (app fname (map parse-expr x)) ]
))




;CREAMOS TIPO PROG
;<prog>   ::= {<fundef>* <expr>}
(deftype prog
  (program list-fun expr ))

; parse-prog :: src -> prog
;parsea el programa, es decir lo pasa de sintaxis concreta a abstracta
(define (parse-prog src )
 
  (match src
     [(list x ... n) (program (map parse-expr x) (parse-expr n))] )
 )





;CREACION TIPOS ENV
;<env>:: (mtEnv)
;       | (aEnv <id> <val> <env>)

  (deftype Env
  (mtEnv)
  (aEnv id val type env))

;CREAMOS LOS AMBIENTES
(define empty-env (mtEnv))
(define extended-env aEnv)


;env-lookup: val Env -> val
;función que bisca en el ambiente un id y devuelve su valor
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val type rest)
     (if (eq? id x)
         val
         (env-lookup x rest))]))


;interp :: expr -> val
;interpreta/calcula nuestra sintaxis abstracta y devuelve un valor
(define (interp-expr expr listfun env)
  (match expr
    [(num n) n]
    [(bool l) l]
    [(id y) (env-lookup y env)] ;aqui buscar el id y si esta lo devuelve (el numero) 
    [(binop op l r)
     (let (( der  (interp-expr r listfun env))
           (izq (interp-expr l listfun env) ))
       (cond
         [(equal? op +)  (if (or (boolean? izq) (boolean? der))  (error "Runtime type error: expected Number found Boolean") (op izq der) ) ]
         [(equal? op -)  (if (or (boolean? izq) (boolean? der))  (error "Runtime type error: expected Number found Boolean") (op izq der) ) ]
         [(equal? op *)  (if (or (boolean? izq) (boolean? der)) (error "Runtime type error: expected Number found Boolean")   (op izq der) ) ]
         [(equal? op /)  (if (or (boolean? izq) (boolean? der))  (error "Runtime type error: expected Number found Boolean")  (op izq der) ) ]
         [(equal? op <)  (if (or (boolean? izq) (boolean? der)) (error "Runtime type error: expected Number found Boolean")   (op izq der) ) ]
         [(equal? op >) (if (or (boolean? izq) (boolean? der))  (error "Runtime type error: expected Number found Boolean")  (op izq der) ) ]
         [(equal? op =) (if (or (boolean? izq) (boolean? der)) (error "Runtime type error: expected Number found Boolean")  (op izq der) ) ]
         [else   (if (or (num? izq) (num? der))  (error "Runtime type error: expected Boolean found Number")  (op izq der) ) ]
         )) ]
    [(unop op l)
     (let (( val  (interp-expr l listfun env )  ))
       (cond
         [(equal? op add1)     (if (boolean? val)   (error "Runtime type error: expected Number found Boolean") val )]  
         [(equal? op add1)     (if (boolean? val)   (error "Runtime type error: expected Number found Boolean")  val )]  
         [(equal? op sub1)     (if (boolean? val)   (error "Runtime type error: expected Number found Boolean")  val )]  
         [(equal? op not)      (if (number? val)    (error "Runtime type error: expected Boolean found Number")   val )]  ))]
         
    [(with listidv body)                    ;(with (list (list 'x 'Num (num 5)) (list 'y 'Num (num 7)) (list 'z 'Num (num 9))) (id 'x)))
     (interp-expr body listfun (guardar-id-val listidv env listfun ))]
    [(iff cond t f)
    (let (( condicion (interp-expr cond listfun env) )
          ( trues (interp-expr t listfun env))
          ( falses (interp-expr f listfun env)))
      (if condicion trues falses ))]
                
                       
     [(app fname  listnum)
      (def (fundef name listarg type body) (lookup-fundef fname listfun))
      (def new-env (guardar-id-val-fun listarg listnum env empty-env listfun))
      (verificar-contratos listarg new-env listfun)
      (interp-expr body listfun new-env )]
)
  )

;verificar-contratos:: List[]-> error/true
;funcion que verifica los contratos de todos los arg de la funcion a ocupar   
(define (verificar-contratos listarg env listfun)    ;   (list (arg0 'x (Num) 'positive) (...) ...)
  (if (empty? listarg)
      '()
      (let ((argu (first listarg)))
        (let (( contrato (arg0-contract argu))); nombre del contrato
          (if (equal? 'NOCONTRACT contrato)    ;si no hay contrati va a la siguiente
                  (verificar-contratos (cdr listarg) env listfun)
                  (let ((valorx (env-lookup (arg0-id argu) env))  ;valor de la variable
                        (tipo (arg0-type argu ))
                        (contfun (lookup-fundef contrato listfun)))
                    (def (fundef name listargu tipofun body) (lookup-fundef contrato listfun))  ;la funcion del contrato
                    (def mi-env (extended-env 'x valorx (arg0-type argu)  empty-env)) 
                    (let (( valor (interp-expr body listfun mi-env )))
                      (cond
                        [(not (boolean? valor)) (error (error (format "Runtime contract error: ~a does not satisfy ~a"(arg0-id argu) contrato)    ))] ;si el resultado no es bool
                        [(not(Bool? tipofun))  (error (error (format "Runtime contract error: ~a does not satisfy ~a"(arg0-id argu) contrato)))]
                        [(equal? #f valor ) (error (format "Runtime contract error: ~a does not satisfy ~a"(arg0-id argu) contrato)     )])))))
        (verificar-contratos (cdr listarg) env listfun))))

  



;guaradar-id-val:: List[cons] Env List[fundef] -> Env
;Dada dos listas de pares que contienen un identidficador y una expresion , se extiende el ambiente agregando estosvalores
(define (guardar-id-val list env  listfun)  
  (if (empty? list)
      env                         
      (let ((p (first list)))    ;(list 'x 'Num (num 5)
        ( guardar-id-val (cdr list) (extended-env (car p ) (interp-expr (third p) listfun env) (second p)  env) listfun) )
      )
  )

;guaradar-id-val-fun:: List[atom] List[expr] Env Env List[fundef] -> Env
;dada dos listas uan con identificadores y otra de los respectivos valores de estos mismos, extiende el ambiente env-dentrof
;con los valoreas antes mensionados
  (define (guardar-id-val-fun listarg listnum env-val env-dentrof listfun)
    (if (empty? listarg)
        env-dentrof
        (let ((argu (first listarg))
              (val (first listnum)))
        (guardar-id-val-fun  (cdr listarg)  (cdr listnum)  env-val (extended-env (arg0-id  argu) (interp-expr val listfun env-val) (arg0-type argu) env-dentrof) listfun) )
      )
  )


;-------DESDE AQUI EMPIEZA TYPECHECK-------------------------------------------------------



;env-lookup; id Env ->Type
;función que busca en el ambiente el typo del id 
(define (env-lookup-check x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val type rest)
     (if (eq? id x)
         type
         (env-lookup-check x rest))]))

;type-with-arg: List[list] -> error/List 
;funcion que verifica que en el with todos sus argumentos cumplan con el tipo
(define (type-with listarg)
  (if (empty? listarg)
      listarg
      (let (( arg (first listarg) ))
        (let (( verdadero (expr-check (third arg) '() empty-env) ))
          (cond
            [(Num? ( second arg) )   (cond
                                     [(Num? verdadero)   (type-with (cdr listarg))]  ; esta bien va a  el siguiente
                                     [(Bool? verdadero) (error "Static type error: expected Num found Bool") ]
                                     [(Any? verdadero) (type-with (cdr listarg))])] ;esta bien va al siguiente
           [(Bool? ( second arg) )   (cond
                                     [(Num? verdadero)   (error "type error: No coinciden los tipos")]
                                     [(Bool? verdadero)(type-with (cdr listarg))] ;esta bien va al siguiente  ]
                                     [(Any? verdadero) (type-with (cdr listarg))])] ;esta bien va al siguiente  ]
           [(Any? ( second arg) ) (cond
                                     [(Num? verdadero)  (type-with (cdr listarg))] ;esta bien va al siguiente  ]
                                     [(Bool? verdadero) (type-with (cdr listarg))] ;esta bien va al siguiente  ]
                                     [(Any? verdadero)  (type-with (cdr listarg))] ;esta bien va al siguiente  ]
          )])))))
             
;guada-val : List[cons] -> Env
;Función que guarda en un env los arg (y sus tipos) de las funciones
(define (guarda-val listfun env)               
  (if (empty? listfun)
      env
      (let ((p (first listfun)))
        (guarda-val (cdr listfun ) (extended-env (first p) 'nose (third p) env))))      
   )

;guada-val-f : List[cons] -> Env
;Función que guarda en un env los arg (y sus tipos) de las funciones pero este es solo para funciones a diferencia del anterior tiene arg
(define (guarda-val-f listfun env)               
  (if (empty? listfun)
      env
      (let ((p (first listfun)))
        (guarda-val-f (cdr listfun ) (extended-env (arg0-id p) 'nose (arg0-type p) env))))  )    
   




;guardar-val-fun check:: List[] List[expr] Env Env List[fundef] -> Env
;dada dos listas uan con identificadores y otra de los respectivos valores de estos mismos, extiende el ambiente env-dentrof
;con los valoreas antes mensionados
  (define (guardar-val-fun-c listarg listnum env-val env-dentrof listfun)
    (if (empty? listarg)
        env-dentrof
        (let ((arg (first listarg))
              (val (expr-check (first listnum) listfun env-val)))
         (cond
           [(Num? (arg0-type arg))   (if (Bool? val)    (error "Static type error: expected Num found Bool")'bien )   ]
           [(Bool? (arg0-type arg))  (if (Num? val )  (error "Static type error: expected Bool found Num")  'bien  ) ]
           [(Num? val)   (if (Bool? (arg0-type arg))   (error "Static type error: expected Num found Bool") 'bien )  ]
           [(Bool? val)  (if (Num? (arg0-type arg))   (error "Static type error: expected Bool found Num") 'bien   ) ])
                   
        (guardar-id-val-fun  (cdr listarg)  (cdr listnum)  env-val (extended-env (arg0-id arg) 'nonosimportaelvalor val env-dentrof) listfun) )
      )
  )

;cantidad-arg: List[] Lis[] -> error/'bien
;funcion que verifica que la cantidad de arg dados a un afuncion coincide con la cantidad de finida en la funcion
(define (cantida-arg listaval listarg)
             (let ((largo1 (largo listaval))
                   (largo2 (largo listarg)))
              (if (= largo1 largo2) 'bien (error "error en la cantidad de argumentos dada a la funcion"))))
      


;typeof :Expr -> Type (o error)
(define (expr-check expr listfun env)
  (match expr
    [(num n) (Num)]
    [(id x)  (env-lookup-check x env)] ;suponiendo q siempre se remplazan los id por numero
    [(bool l) (Bool)]
    [(unop op l) (let ((val (expr-check   l listfun env)))
                   (cond
                     [(equal? op add1)     (if (or (Num? val)(Any? val)) (Num) (error "Static type error: expected Num found Bool"))  ]
                     [(equal? op sub1)     (if (or (Num? val)(Any? val)) (Num) (error"Static type error: expected Num found Bool")) ]
                     [(equal? op not)      (if (or(Bool? val)(Any? val)) (Bool) (error "Static type error: expected Bool found Num"))]
                     )
                   )]
    [(binop op l r) (let ((valder (expr-check  r listfun env))
                          (valizq (expr-check  l listfun env)))
                      (cond
                        [(equal? op +)     (if (or (Bool? valizq) (Bool? valder))  (error "Static type error: expected Num found Bool")(Num)  ) ]       
                        [(equal? op -)     (if (or (Bool? valizq) (Bool? valder)) (error "Static type error: expected Num found Bool")  (Num) ) ]
                        [(equal? op *)     (if (or (Bool? valizq) (Bool? valder)) (error "Static type error: expected Num found Bool") (Num) ) ]
                        [(equal? op /)     (if (or (Bool? valizq) (Bool? valder)) (error "Static type error: expected Num found Bool") (Num) ) ]
                        [(equal? op <)     (if (or (Bool? valizq) (Bool? valder)) (error "Static type error: expected Num found Bool")  (Bool) ) ]
                        [(equal? op >)     (if (or (Bool? valizq) (Bool? valder)) (error "Static type error: expected Num found Bool") (Bool) ) ]
                        [(equal? op =)     (if (or (Bool? valizq) (Bool? valder)) (error "Static type error: expected Num found Bool") (Bool) ) ]
                        [else  (if (or (Num? valizq) (Num? valder))(error "Static type error: expected Bool found Num")  (Bool) ) ]
                        ))]
    [(iff c tr fa ) (let (( condi (expr-check c listfun env))
                        ( t  (expr-check tr listfun env))
                        ( f  (expr-check fa listfun env)))
                    (cond
                      [(Num? condi) (error "Static type error: expected Bool found Num")]
                      
                      [(equal? t f)  (if (Any? t) (Any) (if (Num? t) (Num) (Bool)) )]    ;caso en que ambos son iguales                                                    ]
                      [(not (equal? t f)) (error "type error: expected Both be the same type") ]
                      [else (Any)])) ];todos los casos que quedan serán Any

    
    [(with listval body)
     (type-with  listval )                                ; cehcque que los tipo id del with coincida lo que dice conel valor
     (expr-check body listfun (guarda-val listval env) )]
    [(app fname listval )
     (def (fundef name listarg type body)  (lookup-fundef fname listfun))    ;buscamos la funcion
     (cantida-arg listval listarg)                           ;numero de argumentos coincide
     (guardar-val-fun-c listarg listval env empty-env listfun ) ;veamos q los tipos q le paasamos coinciden con los q deberian ser
      type]

    )
  )


;funcheck List[FunDef] -> "todobien/error
;funcion que chechequea todas las funciones de un programas y checkea si el retorno coincide con el dicho previamente
;si todo está correcto al terminar de recoorer la lista hay un str "todo bien"
(define (funcheck  listfun listttt )
  (if (empty? listfun)
      "todobien"
      (let (( fun (first listfun)))
        (let (( b (expr-check (fundef-body fun)  listttt  (guarda-val-f (fundef-list-param fun) empty-env))))
          (funcion-revisa-contratos (fundef-list-param fun) listttt)
          (cond
            [(Bool? (fundef-type-result fun))  (if (equal? b (or (Bool) (Any)) ) b (error "type error: expected Bool got Num") ) ]
            [(Num? (fundef-type-result fun))  (if (equal? b (or (Num) (Any)) ) b (error "type error: expected Num got Bool" ))]
            [(Any? (fundef-type-result fun))  b ])
          (funcheck (cdr listfun) listttt)))
        ))

;funcion que ira revisando los contratos de los argumentos yd e existir revisara que este funcion retorne un nbool y que solo se pueda recibir un nvalor
(define(funcion-revisa-contratos arg listfun )
  (if (empty? arg)
      '()
      (let (( argu(first arg) ))
         (let (( contrato (arg0-contract argu)))        ;contrato
           (cond
             [  ( equal? contrato 'NOCONTRACT)  (funcion-revisa-contratos (cdr arg) listfun ) ]
             [else
              (def (fundef name listarg type body)  (lookup-fundef contrato listfun))         ;buscamos la funcion
              (cond
                [(not (equal?(Any) (arg0-type argu))) (error (format "Static contract error: invalid type for ~a" contrato))]  ;si el argumento no es de tipo any falla
                [(not (Bool? type))        (error(format "Static contract error: invalid type for ~a" contrato)) ]
                [(> (largo listarg) 1)   (error(format "Static contract error: invalid type for ~a" contrato)) ] )]))
        (funcion-revisa-contratos (cdr arg) listfun ))
      ))          
           




;ret-lit: Type -> 'Num/'Any/'Bool
;funcion que dependiendo de la el typo retorna el literal correspondiente
(define (ret-lit type )
  (cond
    [(Num? type) 'Num]
    [(Any? type) 'Any]
    [(Bool? type) 'Bool]))

;typecheck; src -> 'Num/'Bool/'Any/"error"
;función que recibe sintaxis concreta que llamando a otras funciones checkeo que los tipos esten bien, de caso contrario tira un error 
(define (typecheck src)
  (let (( prog (parse-prog src)))                          ;parsea el programa   
    (let (( listfun (program-list-fun prog ) )          ; listfun = (list (fundef ...) (fundef ...) ...)
          ( expr (program-expr prog)  ))                ;expr = (   ...   )
      (funcheck listfun listfun)                              ; chequea las funcionesestan bien definidas
      (ret-lit (expr-check expr listfun empty-env))  ;se verifica que la expresion esta bien y se imprime el tipo de esta
      
    )))







;----INTERPRETE Y RUN ---------------------------------------------------------------------

;interp-prog prog: prog -> val
;Con el programa ya parseado interpreta la expresion pasandoles la lista de funciones y un ambiente vacio
(define (interp-prog prog)
  (interp-expr (program-expr prog) (program-list-fun prog)  empty-env))


(define flag #t) ;si se quiere ocupar el typecheck
;(define flag #f) ; no se quiere ocupar el typecheck

;run : src -> val
;fución que junta toda las partes parseo, checkeo (si esque se quiere o no) y interprete. se le pasa la sintaxis concreta y devuelve el valor
(define (run prog)
  (if flag
      (let (( programa (parse-prog prog)))
        (let (( listfun (program-list-fun programa ) )
              ( expr (program-expr programa)  ))
          (funcheck listfun listfun)                    ;cheque las funciones
          (expr-check expr listfun empty-env)   ;chequea la expresion
          (interp-prog  programa)))
      (interp-prog(parse-prog prog) );caso sin el chequeo
      ))




 