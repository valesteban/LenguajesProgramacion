#lang play
(require "t1.rkt")


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


(test/exn (run '{{with ((x #t)) {+ 1 x }}}) "error")
(test/exn (run '{{with ((x #t)) {- 1 x }}}) "error")
(test/exn (run '{{with ((x #t)) {< 1 x }}}) "error")
(test/exn (run '{{with ((x #t)) {> 1 x }}}) "error")
(test/exn (run '{{with ((x #t)) {* 1 x }}}) "error")
(test/exn (run '{{with ((x #t)) {/ 1 x }}}) "error")
(test/exn (run '{{with ((x #t)) {&& 1 x }}}) "error")
(test/exn (run '{{with ((x #t)) {add1 x }}}) "error")
(test/exn (run '{{with ((x #t)) {sub1 x }}}) "error")
(test/exn (run '{{with ((x 1)) {! x }}}) "error")


(test (run '{{with ((x 3)) {+ 1 x }}}) 4)
(test (run '{{with ((x 3)) {- 1 x }}}) -2)
(test (run '{{with ((x 3)) {< 1 x }}}) #t)
(test (run '{{with ((x 3)) {> 1 x }}}) #f)
(test (run '{{with ((x 3)) {* 1 x }}}) 3)
(test (run '{{with ((x 3)) {/ 1 x }}}) 1/3)
(test (run '{{with ((x #t)) {&& #t x }}}) #t)
(test (run '{{with ((x 3)) {add1 x }}}) 4)
(test (run '{{with ((x 3)) {sub1 x }}}) 2)
(test (run '{{with ((x #t)) {! x }}}) #f)
(test (run '{{with ((x 3)) {if #t  x 2}}}) 3)

;A UNA FUNCION QUE TOMA 3 NUMEROS Y LOS SUMAS VAOS A IR CAMBIANDO ESTOS VALORES QUE RECIBE
(test (run '{{define {sum x y z} {+ x {+ y z}}}
        {sum 1 2 3}}) 6)

(test/exn (run '{{define {sum x y z} {+ x {+ y z}}}
        {sum #t 2 3}}) "error")                      ;recibe un #t en ves de un numero

(test (run '{{define {sum x y z} {+ x {+ y z}}}
        {sum 1 {add1 4} 3}}) 9)                      

(test/exn (run '{{define {sum x y z} {+ x {+ y z}}}
        {sum 1 {&& #t #t} 3}}) "error")             ;recibe un #t en ves de un numero

(test/exn (run '{{define {sum x y z} {+ x {+ y z}}} {define {false x} {&& x #f }}
        {sum 1 2 {false #t}}}) "error")              ;recibe un #f (a traves de una funcion) en ves de un numero

(test/exn (run '{{define {sum x y z} {+ x {+ y z}}} {define {false x} {&& x #f }}
        {sum 1 2 {false 1}}}) "error")              ;funcion que se ocupa para obtener un argumento para la suma recibe un 1 en ves de un valor boolean


(test (run '{{define {boleano y z} {&& y z}} 
        {boleano #t #t}}) #t)

(test (run '{{define {boleano y z} {&& y z}} 
        {boleano #t {! #t}}}) #f)

(test (run '{{define {boleano y z} {&& y z}} {define {false x} {&& x #f }}
        {boleano #t {false #t}}}) #f)  

(test/exn (run '{{define {boleano y z} {&& y z}} 
        {boleano #t 1}}) "error")               ;recibe un 1 en ves de un boleano

(test/exn (run '{{define {boleano y z} {&& y z}} 
        {boleano #t {add1 3}}}) "error")        ;recibe un 4 en ves de un boleano


(test/exn (run '{{define {boleano y z} {&& y z}} {define {false x} {&& x #f }}
        {boleano #t {false 1}}}) "error")       ;funcion false recibe un numero en ves de un bolean

;PROGRAMAS DEL ENUNCIADO

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

(test (run '{
   {define {triple x} {* 3 x}}
   {define {add2 x} {+ 2 x}}
   {add2 {triple 2}}
}) 8)

(test (run '{{with {{x 5} {y  10}} {+ x y}}}) 15 )

(test (run '{{define {gt42 x} {> x 42}}
 {gt42 43}}) #t)

(test (run '{{define {id x } x}
 {id 5}}) 5)

(test/exn (run '{{define {add2 x } {+ x 2}}
 {with {{oops #f}}
   {add2 oops}}}  ) "error")





