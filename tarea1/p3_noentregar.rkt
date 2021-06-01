#lang play

(print-only-errors #t)
(define w1 '{with {{x 5}{y 7} x}})
(define w2 '{with {{x : Num 5} {y : Num 7} x}})

;VER EL LARGO D EUNA LISTA
(define (largo lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))

;DEFINICION DE TYPOS
(deftype Type
  [Num]
  [Bool]
  [Any])

;DEFINICION DE ARG
(deftype Arg
  [arg0 x ]
  [arg1 y type]
  [arg2 z type contract])


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

; TYPE CHECKER 
(define (is-binop? x) (member x binops))

;<unop>   ::= ! | add1 | sub1
(define (parse-unop bin)
  (match bin
    ['add1 add1]
    ['sub1 sub1]
    [! not] 
    )
  )
;DEFINICION DE UNOPS
(define unops (list 'add1 'sub1 '!))          

;TYPE CHECKER DE UNOPS
(define (is-unop? x) (member x unops))        



;DEFINICIÓN  DE FUNCIONES
;<fundef> ::= {define {<id> <id>*} <expr>}      
(deftype FunDef
  (fundef name list-param type-result body))


;BUSCA LA FUNCIÓNE N NUESTRA LISTA DE FUNCIONES
;lookup-fundef :: Sym List[FunDef] -> FunDef (o error)
(define (lookup-fundef f fundefs)
  (match fundefs
    ['() (error "undefined function:" f)]
    [(cons fd fds) (if (equal? f (fundef-name fd))
                       fd
                       (lookup-fundef f fds))]))

;PARSE DE LAS EXPRESIONES
;pase-expr: src -> Expr
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
     (cond                          
       [(if (list?(second list1)) (fundef (first list1) (map parse-la (cdr list1)) (Any) (parse-expr exp) ) (fundef (first list1) (map parse-la (cdr list1)) (Any) (parse-expr exp) ))])]
    
    [(list 'define list1 : type  exp ) ; CASO DONDE SI SALE EL VALOR DE SALIDA
     (cond
        [(if (list?(second list1)) (fundef (first list1) (map parse-la (cdr list1)) (ponetipo type) (parse-expr exp) ) (fundef (first list1) (map parse-la (cdr list1)) (ponetipo type) (parse-expr exp) )) ])]

    [(list fname x ...) (app fname (map parse-expr x)) ]
))

;funcion para poner el tipo de cada cosa
(define (ponetipo str)
  (cond
    [(equal? str 'Num ) (Num)]
    [(equal? str 'Bool) (Bool)]
    )
  )

;FUNCION AYUDA A PARSESAR LA LISTA DENTRO DE LA FUNCINO
(define (parse-la arg)  ;(list x y z)
                        ;(list [x : Num] [y : Num ][z : num])
  (if (list? arg)
      (cons (first arg) (ponetipo(third arg)))
      (cons arg (Any))
  ))



;CREAMOS TIPO PROG
;<prog>   ::= {<fundef>* <expr>}
(deftype prog
  (program list-fun expr ))

;PARSE PROGRAMA
; parse-prog :: src -> prog
(define (parse-prog src )
  (match src
     [(list x ... n) (program (map parse-expr x) (parse-expr n))] )
 )


;PARSE Id
(define (parse-id lista )
  (let (( largo (largo lista)))
    (cond
      [(if ( > largo  2) ((car lista) (ponetipo(third lista)) (parse-expr (fourth lista )))    ((car lista) (Any) (parse-expr (second lista)))) ] ; es de la forma nueva
    )
  ))



;CREACION TIPOS ENV
;<env>:: (mtEnv)
;       | (aEnv <id> <val> <env>)

  (deftype Env
  (mtEnv)
  (aEnv id val type env))

;CREAMOS LOS AMBIENTES
(define empty-env (mtEnv))
(define extended-env aEnv)

;FUNCION QUE BUSCA EN EL AMBIENTE UN ID (aEnv (list (cons x (num 1)) (cons y (num2))    (aEnv (list .....  ))    ))
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val type rest)
     (if (eq? id x)
         val
         (env-lookup x rest))]))

;INTERPRETE
;interp :: prog -> val           prog (program   (list funcion1 funcion 2) expr )

(define (interp-expr expr listfun env)
  (match expr
    [(num n) n]
    [(bool l) l]
    [(id y) (env-lookup y env)] ;aqui buscar el id y si esta lo devuelve (el numero)
    [(binop op l r) (cond
                        [(equal? op +)     (if (and (num? l) (bool? r))  (error "type error: expected Num got bool") (op (interp-expr l listfun env)  (interp-expr r listfun env)) ) ]
                        [(equal? op -)     (if (and (num? l) (bool? r))  (error "type error: expected Num got bool") (op (interp-expr l listfun env) (interp-expr r listfun env)) ) ]
                        [(equal? op *)     (if (and (num? l) (bool? r))  (error "type error: expected Num got bool") (op (interp-expr l listfun env) (interp-expr r listfun env))) ]
                        [(equal? op /)     (if (and (num? l) (bool? r))  (error "type error: expected Num got bool") (op (interp-expr l listfun env) (interp-expr r listfun env)) ) ]
                        [(equal? op <)     (if (and (num? l) (bool? r))  (error "type error: expected Num got bool") (op (interp-expr l listfun env) (interp-expr r listfun env)) ) ]
                        [(equal? op >)     (if (and (num? l) (bool? r))  (error "type error: expected Num got bool") (op (interp-expr l listfun env) (interp-expr r listfun env))) ]
                        [(equal? op =)     (if (and (num? l) (bool? r))  (error "type error: expected Num got bool") (op (interp-expr l listfun env) (interp-expr r listfun env)) ) ]
                        [else (if (and (bool? l) (num? r)) (error "type error: expected Bool got Num") (op (interp-expr l listfun env) (interp-expr r listfun env)) )]) ]
    [(unop op l) (cond
                   [(equal? op add1)     (if (bool? l)   (error "type error: expected Num got bool")  (op (interp-expr l listfun env )))]  
                   [(equal? op add1)     (if (bool? l)   (error "type error: expected Num got bool")  (op (interp-expr l listfun env )) )]
                   [(equal? op sub1)     (if (bool? l)   (error "type error: expected Num got bool")  (op (interp-expr l listfun env ))) ]
                   [(equal? op not)      (if (num? l)    (error "type error: expected Bool got Num")  (op (interp-expr l listfun env )) )])]
         
    [(with listidv body)                    ;(with (list (list 'x 'Num (num 5)) (list 'y 'Num (num 7)) (list 'z 'Num (num 9))) (id 'x)))
     (interp-expr body listfun (guardar-id-val listidv env listfun ))]
    [(iff cond t f) (if (interp-expr cond listfun env)
                        (interp-expr t listfun env)
                        (interp-expr f listfun env))]
     [(app fname  listnum)
      (def (fundef name listarg type body) (lookup-fundef fname listfun))
     (interp-expr body listfun (guardar-id-val-fun listarg listnum env empty-env listfun))]

    )
  )

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
        (let ((arg (first listarg))
              (val (first listnum)))
        (guardar-id-val-fun  (cdr listarg)  (cdr listnum)  env-val (extended-env (car arg) (interp-expr val listfun env-val) (cdr arg) env-dentrof) listfun) )
      )
  )
;INTERP PROGRAMA
(define (interp-prog prog)
  (interp-expr (program-expr prog) (program-list-fun prog)  empty-env))







;typeof :Expr -> Type (o error)
(define (typecheck-expr expr)
  (match expr
    [(num n) (Num)]
    [(id x) (Any)] ;suponiendo q siempre se remplazan los id por numero
    [(bool l) (Bool)]
    [(unop op l) (let ((val (typecheck-expr  l)))
                   (cond
                     [(equal? op add1)     (if (Num? val) (Num) (error "type error: expected Num got bool"))  ]
                     [(equal? op sub1)     (if (Num? val) (Num) (error "type error: expected Num got bool")) ]
                     [(equal? op not)      (if (Bool? val) (Bool) (error "type error: expected Bool got Num"))]
                     )
                   )]
    [(binop op l r) (let ((valizq (typecheck-expr l))
                          (valder (typecheck-expr r)))
                      (cond
                        [(equal? op +)     (if (and (Num? valizq) (Num? valder)) (Num) (error "type error: expected Num got bool")) ]
                        [(equal? op -)     (if (and (Num? valizq) (Num? valder)) (Num) (error "type error: expected Num got bool")) ]
                        [(equal? op *)     (if (and (Num? valizq) (Num? valder)) (Num) (error "type error: expected Num got bool")) ]
                        [(equal? op /)     (if (and (Num? valizq) (Num? valder)) (Num) (error "type error: expected Num got bool")) ]
                        [(equal? op <)     (if (and (Num? valizq) (Num? valder)) (Num) (error "type error: expected Num got bool")) ]
                        [(equal? op >)     (if (and (Num? valizq) (Num? valder)) (Num) (error "type error: expected Num got bool")) ]
                        [(equal? op =)     (if (and (Num? valizq) (Num? valder)) (Num) (error "type error: expected Num got bool")) ]
                        [else (if (and (Bool? valizq) (Bool? valder)) (Bool) (error "type error: expected Bool got Num"))]   
                        ))]
    [(iff c t f ) (let (( cond (typecheck-expr c))
                        ( tu  (typecheck-expr t))
                        ( fa  (typecheck-expr f)))
                    (cond
                      [(Num? cond) "error condicion debe ser de tipo bool " ]
                      [(not (equal? tu fa)) (if (or (Any? t) (Any? f)) (Any) "error ambas ramas deben ser del mismo tipo" )]
                      [(equal? tu fa ) (tu)])) ]
    
    [(with listval body)
     (map typechecklista listval)
     (typecheck-expr body)]
    [(app fname listval ) 'nose] ;se asume que no se sabe
    )
  )


;FUNCION QUE CHECHEASI LOS ARUGUMENTOS CON EL TIPO DE WITH

(define (typechecklista arg)  ;(list 'x (Num) (num 7))
  (let (( val (typecheck-expr (third arg)) ))
    ( cond
       [(not ( equal? val (second arg ))) "error arg doesn ́t have the same type as the argument"]))
  )
  
;RUN PARTE 1

(define (run prog)
  (interp-prog (parse-prog prog)))

(define (typecheck-prog arg)
 ( let (( prog (parse-prog arg) ))
    prog))

;RUN PARTE 2
;(define (run2 prog)
;  (let ([expr (program-expr (parse-prog prog))])
;    (let ([ t (typecheck  expr)])
;      (interp-prog (parse-prog prog)))
;    )
;  )
(define notbien (unop not (bool #t)))
(define notmal (unop not (num 2)))
;(typecheck-expr notbien)
;(typecheck-expr notmal)
(define a (with (list (list 'oops (Any) (bool #f))) (id 'oops)))
(typecheck-expr a)

;(define f (list (fundef 'add2 (list (cons 'x (Num))) (Any) (binop + (id 'x) (num 2)))))


;(define t2 (program (list (fundef 'gt42 '((x . Any)) 'Bool (binop > (id 'x) (num 42)))) (app 'gt42 (list (num -41)))))
;(define t3 (program (list (fundef 'id '((x . Num)) 'Any (id 'x))) (app 'id (list (num 5)))))

