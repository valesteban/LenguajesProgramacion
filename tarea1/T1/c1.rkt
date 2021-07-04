#lang play
#| <formula> ::=
| (Bool <Bool>)
| (id <sym>)
| (^ <formula> <formula>)
| (v <formula> <formula>)
| (with <sym> <formula> <formula>)
|#

;a) Escriba utilizando deftype el tipo inductivo Formula que genera los AST de los
;programas de este lenguaje.
(deftype Expr-mio
  [bool-mio b]
  [id-mio d]
  [y-mio izq der]
  [or-mio izq der]
  [with id val body])

;b) Escriba utilizando deftype el tipo FValue que representa los valores de este lenguaje.
(deftype FValue
  [boolV b])

;d) Escriba la gramática BNF de las s-expr que representan la sintaxis concreta del
;lenguaje.

#| <formula> ::=
| (Bool <Bool>)
| (id <sym>)
| (^ <formula> <formula>)
| (v <formula> <formula>)
| (with <sym> <formula> <formula>)
|#

;e) Escriba el parser
; parse :: s-expr -> Formula
(define (parse-mio s-expr)
  (match s-expr
    [(? symbol?) (cond
                   [(or (equal? s-expr 'True ) (equal? s-expr 'True )) (id-mio s-expr)   ]
                   [else   (id-mio s-expr)])]
    [(list '^ izq der) (y-mio (parse-mio izq) (parse-mio der))]
    [(list 'v izq der) (or-mio (parse-mio izq) (parse-mio der))]
    [(list 'with (list x val) body )
     (with  x (parse-mio val)(parse-mio body))]
    ))

(define (interp-mio expr )
  (match expr
    [(bool-mio b) b ]
    [(y-mio izq der ) (and (interp-mio izq) (interp-mio der))]
    [(or-mio izq der ) (or (interp-mio izq) (interp-mio der))]
    [(with x val body)
     (interp-mio (subst x (bool-mio (interp-mio val)) body ))]
    ))

(define (subst id val body)
  (match body
    [(bool-mio b) body]
    [(id-mio y) (if (equal? y id) val body)]
    [(y-mio izq der ) (and (subst id val izq) (subst id val der))]
    [(or-mio izq der ) (or (subst id val izq) (subst id val der))]
    [(with y nv b)
     (with y (subst id val nv)(if (eq? id y)
                                  b
                                  (subst id val b)))]))

;(test (parse 'True)(id-mio 'True))
;(test (parse ' (v A False)) (or-mio (id-mio 'A) (id-mio 'False)))
;(test (parse ' (^ C (v A B))) (y-mio (id-mio 'C) (or-mio (id-mio 'A) (id-mio 'B))))
;(test (parse ' (with (A True) (^ A A) )) (with 'A (id-mio 'True) (y-mio (id-mio 'A) (id-mio 'A))))


;p3)
;Disponemos de un dispositivo liviano cuyo procesador es basado en una pila y que
;solamente soporta las siguientes instrucciones:
;• push num: pone el número num en la pila                 [1,2,3,4]  -> push 7 ->  [7,1,2,3,4]
;• add: adiciona los dos números encima de la pila         [1,2,3,4]  -> add    ->  [3,3,4]
;• sub: subtrae los dos números encima de la pila          [1,2,3,4]  -> sub    ->  [-1,3,4]
 
;Definan en PLAI Scheme un compilador (parser + generador de código) del lenguaje
;AE al procesador basado en pila. El output del compilador es la lista (plana) de
;instrucciones a ejecutar por el procesador. Por ejemplo:
#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r))
 
#|
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
|#
;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]))
 
;; calc :: Expr -> number
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]))
 
;; run :: s-expr -> number
(define (run prog)
  (calc (parse prog)))

; (compile ’(+ 3 5))
;(push 3 push 5 add)
(parse '(+ 3 5))

(define (compile s-expr)
   (let ([  p (parse s-expr)   ])
     (compile-parte2 p)))

(define (compile-parte2 s-expr)
  (list (match s-expr
          [(num n ) (format "push ~a" n)]
          [(add l r)(format "add ~a ~a " (compile-parte2 l)(compile-parte2 r)  )]
          [(add l r)(format "sub ~a ~a " (compile-parte2 l)(compile-parte2 r)  )]
          )))


(compile 3)
(compile '(+ 1 2))