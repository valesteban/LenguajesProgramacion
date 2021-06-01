#lang play
(require "t1.rkt")

;EJEMPLO COMO PARSEA PARA DIFERENTES FORMAS DEFINIR LA MISMA FUNCION PERO DIFERENTE FORMA EN EL ARGUMENTO

(test (parse-expr '{define {sum {x : Num @ positive} {y : Num @ positive} {z : Num @ positive}} {+ x {+ y z}}}) (fundef
 'sum
 (list (arg0 'x (Num) 'positive) (arg0 'y (Num) 'positive) (arg0 'z (Num) 'positive))
 (Any)
 (binop + (id 'x) (binop + (id 'y) (id 'z)))))

(test (parse-expr '{define {sum {x : Num} {y : Num} {z : Num }} {+ x {+ y z}}}) (fundef
 'sum
 (list (arg0 'x (Num) 'NOCONTRACT) (arg0 'y (Num) 'NOCONTRACT) (arg0 'z (Num) 'NOCONTRACT))
 (Any)
 (binop + (id 'x) (binop + (id 'y) (id 'z)))))

(test (parse-expr '{define {sum x y z} {+ x {+ y z}}})
      (fundef 'sum  (list (arg0 'x (Any) 'NOCONTRACT) (arg0 'y (Any) 'NOCONTRACT) (arg0 'z (Any) 'NOCONTRACT))
 (Any)
 (binop + (id 'x) (binop + (id 'y) (id 'z)))))

;EJEMPLO COMO SE PARSEA UN PROGRAMA

(test (parse-prog '{{define {gt42 x} : Bool {> x 42}}
                     {gt42 43}})
      
      (program (list (fundef 'gt42 (list (arg0 'x (Any) 'NOCONTRACT)) (Bool) (binop > (id 'x) (num 42))))
               (app 'gt42 (list (num 43)))))

(test (parse-prog '{{define {positive x} : Bool {> x 0}}
                    {define {div {x : Num @ positive} y}
                      {/ y x}}{div 5 3}})
      (program
       (list (fundef 'positive (list (arg0 'x (Any) 'NOCONTRACT)) (Bool) (binop > (id 'x) (num 0)))
             (fundef 'div (list (arg0 'x (Num) 'positive) (arg0 'y (Any) 'NOCONTRACT)) (Any) (binop / (id 'y) (id 'x))))
       (app 'div (list (num 5) (num 3)))))

;EJEMPLOS DEL ENUNCIADO CON LOS ERRORES POSIBLES

(test (run '{{define {positive x} : Bool {> x 0}}
             {define {div {x  @ positive} y}{/ y x}}
             {div 5 3}} ) 3/5)


(test/exn (run '{{define {positive x} : Bool {> x 0}}
                 {define {div {x : Num @ positive} y}{/ y x}}
                 {div 5 3}}) "error")                ;error porque a la funcion contrato se le pasa un argumento que no es de tipo Any sino Num

(test/exn (run '{{define {positive x} : Num {> x 0}}
                 {define {div {x  @ positive} y}{/ y x}}
                 {div 5 3}}) "error")                 ;error porque el contrato dice que devuelve un Num

(test/exn (run '{{define {positive x} {> x 0}}
                 {define {div {x  @ positive} y}{/ y x}}
                 {div 5 3}}) "error")                    ;error porque el contrato no dice que se devuelve un bool (y por lo tanto el tipo que se devuelve es un Any)

(test/exn (run '{{define {positive x} : Bool {> x 0}}
                 {define {div {x  @ positive} y} {/ y x}}
                 {div -5 3}}) "error")                  ;error -5 no cumple con el contrato

(test/exn (run '{{define {positive x} : Num {> x 0}}
                 {define {div {x  @ positive} y @positive}{/ y x}}
                 {div 5 -3}}) "error") ;error porque -3 no cumple con el contrato


(test/exn (run '{{define {add x y} : Num {+ x y}}
                 {define {oh-no {x @ add} y} #t}
                 {oh-no 21 21}}) "error")              ;el contracto recibe dos parametros

(test (run '{{define {lt100 x} {< x 100}}
             {define {positive x} : Bool {> x 0}}
             {define {percentage? x} : Bool {&& {lt100 x} {positive x}}}
             {define {calc {x @ positive} {y @ percentage?}}      {/ {* y y} x}}
             {calc 25 3}}) 9/25)

(test/exn (run '{{define {lt100 x} {< x 100}}
                 {define {positive x} : Bool {> x 0}}
                 {define {percentage? x} : Bool {&& {lt100 x} {positive x}}}
                 {define {calc {x @ positive} {y @ percentage?}}      {/ {* y y} x}}
                 {calc -3 10}}) "error")                    ;error porque -3 no es positivo entonce no cumple con el contrato

(test/exn (run '{{define {lt100 x} {< x 100}}
                 {define {positive x} : Bool {> x 0}}
                 {define {percentage? x} : Bool {&& {lt100 x} {positive x}}}
                 {define {calc {x @ positive} {y @ percentage?}}{/ {* y y} x}}
                 {calc 3 -90}}) "error")                    ;error porque -90 no cumple con el contrato

(test/exn (run '{{define {lt100 x} {< x 100}}
                 {define {positive x} : Bool {> x 0}}
                 {define {percentage? x} : Bool {&& {lt100 x} {positive x}}}
                 {define {calc {x @ positive} {y @ percentage?}}      {/ {* y y} x}}
                 {calc 3 900}}) "error") ;error porque 900 no cumple con el contrato no cumple con el contrato






;FUNCION QUE REVISA los argumentos de funciones y ve si cumplen (se le pasa la lista de funciones y la lista de funciones)
;[revisa errores estaticos en las funciones que serán contrato]


(test/exn (funcheck  (list
  (fundef 'add (list (arg0 'x (Any) 'NOCONTRACT) (arg0 'y (Any) 'NOCONTRACT)) (Num) (binop + (id 'x) (id 'y)))
  (fundef 'oh-no (list (arg0 'x (Any) 'add) (arg0 'y (Any) 'NOCONTRACT)) (Any) (bool #t)))
            (list
  (fundef 'add (list (arg0 'x (Any) 'NOCONTRACT) (arg0 'y (Any) 'NOCONTRACT)) (Num) (binop + (id 'x) (id 'y)))
  (fundef 'oh-no (list (arg0 'x (Any) 'add) (arg0 'y (Any) 'NOCONTRACT)) (Any) (bool #t))))  "error")   ;error porque la funcion add reecibe dos argumentos



;{define {positive x} : Num {> x 0}}
;{define {div {x  @ positive} y }{/ y x}

(test (funcheck  (list
  (fundef 'positive (list (arg0 'x (Any) 'NOCONTRACT)) (Bool) (binop > (id 'x) (num 0)))
  (fundef 'div (list (arg0 'x (Any) 'positive) (arg0 'y (Any) 'NOCONTRACT) (arg0 '@positive (Any) 'NOCONTRACT)) (Any) (binop / (id 'y) (id 'x))))
            
  (list
  (fundef 'positive (list (arg0 'x (Any) 'NOCONTRACT)) (Bool) (binop > (id 'x) (num 0)))    ;error porque esto devuelve un bool 
  (fundef 'div (list (arg0 'x (Any) 'positive) (arg0 'y (Any) 'NOCONTRACT) (arg0 '@positive (Any) 'NOCONTRACT)) (Any) (binop / (id 'y) (id 'x)))) ) empty)

(test/exn (funcheck  (list
  (fundef 'positive (list (arg0 'x (Any) 'NOCONTRACT)) (Bool) (binop > (id 'x) (num 0)))
  (fundef 'div (list (arg0 'x (Bool) 'positive) (arg0 'y (Any) 'NOCONTRACT) (arg0 '@positive (Any) 'NOCONTRACT)) (Any) (binop / (id 'y) (id 'x))))
            
  (list
  (fundef 'positive (list (arg0 'x (Any) 'NOCONTRACT)) (Bool) (binop > (id 'x) (num 0)))    ;error porque esto devuelve un bool 
  (fundef 'div (list (arg0 'x (Bool) 'positive) (arg0 'y (Any) 'NOCONTRACT) (arg0 '@positive (Any) 'NOCONTRACT)) (Any) (binop / (id 'y) (id 'x)))) ) "error") ;error x con el contrato positive es bool y no un any 


(test/exn (funcheck  (list
  (fundef 'positive (list (arg0 'x (Any) 'NOCONTRACT)) (Bool) (binop > (id 'x) (num 0)))
  (fundef 'div (list (arg0 'x (Num) 'positive) (arg0 'y (Any) 'NOCONTRACT) (arg0 '@positive (Any) 'NOCONTRACT)) (Any) (binop / (id 'y) (id 'x))))
            
  (list
  (fundef 'positive (list (arg0 'x (Any) 'NOCONTRACT)) (Bool) (binop > (id 'x) (num 0)))    ;error porque esto devuelve un bool 
  (fundef 'div (list (arg0 'x (Num) 'positive) (arg0 'y (Any) 'NOCONTRACT) (arg0 '@positive (Any) 'NOCONTRACT)) (Any) (binop / (id 'y) (id 'x)))) ) "error") ;error x con el contrato positive es num y no un any 

