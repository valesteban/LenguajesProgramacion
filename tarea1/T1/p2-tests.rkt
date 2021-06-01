#lang play
(require "t1.rkt")

(test (typecheck '{3}) 'Num)
(test (typecheck '{#t}) 'Bool)

(test (typecheck '{{+ 1 4}}) 'Num)
(test/exn (typecheck '{{+ 1 #t}}) "error")
(test/exn (typecheck '{{+ #t 2}}) "error")

(test (typecheck '{{- 1 9}}) 'Num)
(test/exn (typecheck '{{- 1 #t}}) "error")
(test/exn (typecheck '{{- #t #t}}) "error")

(test (typecheck '{{< 1 4}}) 'Bool )
(test/exn (typecheck '{{< 1 #t}}) "error")
(test/exn (typecheck '{{< #t #t}}) "error")

(test (typecheck '{{> 1 4}}) 'Bool )
(test/exn (typecheck '{{> 1 #t}}) "error")
(test/exn (typecheck '{{> #t #t}}) "error")


(test (typecheck '{{= 1 4}}) 'Bool )
(test/exn (typecheck '{{= 1 #t}}) "error")
(test/exn (typecheck '{{= #t #t}}) "error")

(test (typecheck '{{* 1 4}}) 'Num )
(test/exn (typecheck '{{* 1 #t}}) "error")
(test/exn (typecheck '{{* #t #t}}) "error")

(test (typecheck '{{/ 1 4}}) 'Num )
(test/exn (typecheck '{{/ 1 #t}}) "error")
(test/exn (typecheck '{{/ #t #t}}) "error")

(test (typecheck '{{add1 4}}) 'Num) 
(test/exn (typecheck '{{add1 #t}}) "error")

(test (typecheck '{{sub1 4}}) 'Num)
(test/exn (typecheck '{{sub1 #t}}) "error")

(test (typecheck '{{! #t}}) 'Bool)
(test/exn (typecheck '{{! 5}}) "error")

(test (typecheck '{{if #t  1 2}}) 'Num)
(test/exn (typecheck '{{if 1  1 2}}) "error")
(test/exn (typecheck '{{if #t  #t 2}}) "error")


;LOS ARGUMENTOS QUE SE LE PANSAN AL WITH NO COINCIDEN CON LO QUE SON


(test/exn (typecheck '{{with ((x : Num #t)) {+ 1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num #t)) {- 1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num #t)) {< 1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num #t)) {> 1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num #t)) {* 1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num #t)) {/ 1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num 1)) {&& 1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num #t)) {add1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num #t)) {sub1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Bool 1)) {! x }}}) "error")
(test/exn (typecheck '{{with ((x : Bool #t)) {&& 1 x }}}) "error")
(test/exn (typecheck '{{with ((x : Num #t))  {if x  1 2}}}) "error")


(test (typecheck '{{with ((x : Num 1)) {+ 1 x }}}) 'Num)
(test (typecheck '{{with ((x : Num 1)) {- 1 x }}}) 'Num)
(test (typecheck '{{with ((x : Num 2)) {< 1 x }}}) 'Bool)
(test (typecheck '{{with ((x : Num 2)) {> 1 x }}}) 'Bool)
(test (typecheck '{{with ((x : Num 2)) {* 1 x }}}) 'Num)
(test (typecheck '{{with ((x : Num 2)) {/ 1 x }}}) 'Num)
(test (typecheck '{{with ((x : Bool #t)) {&& #t x }}}) 'Bool)
(test (typecheck '{{with ((x : Num 2)) {add1 x }}}) 'Num)
(test (typecheck '{{with ((x : Num 2)) {sub1 x }}}) 'Num)
(test (typecheck '{{with ((x : Bool #t)) {! x }}}) 'Bool)
(test (typecheck '{{with ((x : Bool #t))  {if x  1 2}}}) 'Num)

;MAS EJEMPLO SON EL IF

(test (typecheck '{{if {< 5 6} 1 0 }}) 'Num)
(test (typecheck '{{if {< 5 6} #t #f }}) 'Bool)
(test/exn (typecheck '{{if {< #f 6} 1 0 }}) "error")
(test/exn (typecheck '{{if {+ 7 6} 1 0 }}) "error")
(test/exn (typecheck '{{if {< 5 6} 2 #t}}) "error")
(test/exn(typecheck '{{if 73 #t #t}}) "error")

(test/exn (typecheck '{{with {{x 5} {y : Num #t} {z 42}}z}}) "error") ;no coincidenlos tipos del with
(test/exn (typecheck '{{with {{x : Bool 5} {y : Num 4} {z 42}}z}}) "error") ;;no coincidenlos tipos del with

;EN LA DEFINICION DE FUNCION
(test (typecheck '{{define {f {p : Bool}} {&& p {! p}}}
                          {f {> 3 4}}}) 'Any)
(test (typecheck '{{define {f p } {&& p {! p}}}
                          {f {> 3 4}}}) 'Any)
(test/exn (typecheck '{{define {f {p : Num}} {&& p {! p}}}
                          {f {> 3 4}}}) "error")
(test (typecheck '{{define {f {p : Bool}}: Bool {&& p {! p}}}
                          {f {> 3 4}}}) 'Bool)
(test/exn (typecheck '{{define {f {p : Bool}}: Num {&& p {! p}}}
                          {f {> 3 4}}}) "error")


(test (typecheck '{{define {f p }: Num {+ p {* 3 p}}}
                          {f {add1 4}}}) 'Num)
(test (typecheck '{{define {f p } {+ p {* 3 p}}}
                          {f {add1 4}}}) 'Any)
(test (typecheck '{{define {f {p : Num} } {+ p {* 3 p}}}
                          {f {add1 4}}}) 'Any)
(test/exn (typecheck '{{define {f p } : Bool {+ p {* 3 p}}}
                          {f {add1 4}}}) "error")
(test/exn (typecheck '{{define {f p : Bool } {+ p {* 3 p}}}
                          {f {add1 4}}}) "error")

;EJEMPLOS DE ENUNCIADO

(test (typecheck '{3}) 'Num)

(test (typecheck '{{define {f {p : Bool}} {&& p {! p}}}
                          {f {> 3 4}}}) 'Any)

(test/exn (typecheck '{{define {one {x : Num}} 1}
                          {one #t}}) "error")

(test/exn (typecheck '{{> 10 #t}}) "error")

(test/exn(typecheck '{{if 73 #t #t}}) "error")

(test/exn(typecheck '{{with {{x 5} {y : Num #t} {z 42}}
                            z}}) "error")

;CHEQUEO DENTRO DEL RUN


(test (run '{#t}) #t)
(test (run '{4}) 4)

(test/exn (run '{{+ #f #f}}) "error")
(test/exn (run '{{+ 3 #f}}) "error")
(test (run '{{+ 1 1}}) 2)

(test/exn (run '{{> 1 #f}}) "error")
(test/exn (run '{{< #t #f}}) "error")
(test (run '{{< 4 5}}) #t)

(test/exn (run '{{* 1 #f}}) "error")
(test/exn (run '{{* #t #f}}) "error")
(test (run '{{* 4 5}}) 20)

(test/exn (run '{{/ 1 #f}}) "error")
(test/exn (run '{{/ #t #f}}) "error")
(test (run '{{/ 4 5}}) 4/5)

(test/exn (run '{{= #f #f}}) "error")
(test/exn (run '{{= 1 #f}}) "error")
(test (run '{{= 1 3}}) #f)

(test/exn (run '{{- 1 #f}}) "error")
(test/exn (run '{{- #t #f}}) "error")
(test (run '{{- 1 7}}) -6)

(test (run '{{if #t  1 2}}) 1)

(test/exn (run '{{&& #f 4}}) "error")
(test/exn (run '{{&& 5 5 }}) "error")
(test (run '{{&& #f #t}}) #f)

(test/exn (run '{{add1 #f}}) "error")
(test (run '{{add1 6}}) 7)

(test/exn (run '{{sub1 #f}}) "error")
(test (run '{{sub1 6}}) 5)

(test/exn (run '{{! 1}}) "error")
(test (run '{{! #t}}) #f)


(test/exn (run '{{with ((x : Num #t )) {+ 1 x }}}) "error")
(test/exn (run '{{with ((x : Num  #t)) {- 1 x }}}) "error")
(test/exn (run '{{with ((x : Num #t)) {< 1 x }}}) "error")
(test/exn (run '{{with ((x : Num #t)) {> 1 x }}}) "error")
(test/exn (run '{{with ((x : Num #t)) {* 1 x }}}) "error")
(test/exn (run '{{with ((x : Num #t)) {/ 1 x }}}) "error")
(test/exn (run '{{with ((x : Num #t)) {&& 1 x }}}) "error")
(test/exn (run '{{with ((x : Num #t)) {add1 x }}}) "error")
(test/exn (run '{{with ((x : Num #t)) {sub1 x }}}) "error")
(test/exn (run '{{with ((x : Bool 1)) {! x }}}) "error")


(test (run '{{with ((x : Num 3)) {+ 1 x }}}) 4)
(test (run '{{with ((x : Num 3)) {- 1 x }}}) -2)
(test (run '{{with ((x : Num 3)) {< 1 x }}}) #t)
(test (run '{{with ((x : Num 3)) {> 1 x }}}) #f)
(test (run '{{with ((x : Num 3)) {* 1 x }}}) 3)
(test (run '{{with ((x : Num 3)) {/ 1 x }}}) 1/3)
(test (run '{{with ((x : Bool #t)) {&& #t x }}}) #t)
(test (run '{{with ((x : Num 3)) {add1 x }}}) 4)
(test (run '{{with ((x : Num 3)) {sub1 x }}}) 2)
(test (run '{{with ((x : Bool #t)) {! x }}}) #f)
(test (run '{{with ((x : Num 3)) {if #t  x 2}}}) 3)




(test/exn (run '{{define {sum {x : Num} y z} {+ x {+ y z}}}
        {sum #t 2 3}}) "error")                      ;recibe un #t en ves de un numero                   

(test/exn (run '{{define {sum x y {z : Num}} {+ x {+ y z}}} {define {false x} {&& x #f }}
        {sum 1 2 {false #t}}}) "error")              ;recibe un #f (a traves de una funcion) en ves de un numero

(test/exn (run '{{define {sum x y z} {+ x {+ y z}}} {define {false {x : Bool}} {&& x #f }}
        {sum 1 2 {false 1}}}) "error")              ;funcion que se ocupa para obtener un argumento para la suma recibe un 1 en ves de un valor boolean

;MAS PROGRAMAS DE ENUNCIADO P

(test/exn (run '{{define {sum x y z} {+ x {+ y z}}}
        {define {max x y} {if {< x y} y x}}
        {with {{x 9}}
                 {sum {max x #t} 2 -10} }})  "error") ;valor que le pasamos a la función  max debiera ser un numero

(test/exn (run '{{define {sum x y z} {+ x {+ y z}}}
        {define {max x y} {if {< x y} y x}}
        {with {{x #f}}
                 {sum {max x 6} 2 -10} }}) "error") ;valor que le pasamos al with y que est eluego se lo pasa a la funcipón debiese ser un numero
(test/exn (run '{{define {sum x y z} {+ x {+ y z}}}
        {define {max x y} {if {< x y} y x}}
        {with {{x 9}}
                 {sum {max x 6} #t -10} }}) "error") ;valor que se le pasa a la funcion sum debiese ser un num


(test (run '{ {with {{x 5} {y 7} {z 42}}z}}) 42)

(test (run '{{define {triple x} {* 3 x}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             }) 8)

(test/exn (run '{{define {triple x}: Bool {* 3 x}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             }) "error")


(test (run '{{with {{x : Num 5} {y : Num 10}} {+ x y}}}) 15 )

(test (run '{{define {gt42 x} : Bool {> x 42}}
 {gt42 43}}) #t)

(test (run '{{define {id {x : Num}} x}
 {id 5}}) 5)
(test/exn (run '{{define {add2 {x : Num}} {+ x 2}}
 {with {{oops #f}}
   {add2 oops}}}  ) "error")

