#lang play

(require "main.rkt")
;; Test sub-module.
;; See http://blog.racket-lang.org/2012/06/submodules.html

;no alcance a terminar la tarea, llegue hasta los stream, me funcionaban las cosas de lazy pero cuando llegue a los stream
;tenía una falla que no logré encontrar

;Cosas que asumi :
; pp es solo para listas

;como hice las cosas:
;para extender el lenguajes a lista hice que en la sintaxis conreta al recibir una lista esta la convertia en sintaxis concreta de un cos y luego
;como ya se tenian las cosas para cons no se debiai hacer nada
;lazy: cree un deftipe para guardar las expresiones no evaluadas con su ambiente y luego en puntos de stricten se evaluara (strict es para las funcines y strictver2 para los deftyype)

;this tests should never fail; these are tests for MiniScheme+
(module+ test
  (test (run '{+ 1 1}) 2)

  (test (run '{{fun {x y z} {+ x y z}} 1 2 3}) 6)

  (test (run '{< 1 2}) #t)

  (test (run '{local {{define x 1}}
                x}) 1)

  (test (run '{local {{define x 2}
                      {define y {local {{define x 1}} x}}}
                {+ x x}}) 4)

  ;; datatypes
  (test (run '{local
                {{datatype List
                           {Empty}
                           {Cons a b}}}
                {List? {Empty}}}) #t)

  (test (run '{local {{datatype List
                                {Empty}
                                {Cons a b}}}
                {Empty? {Empty}}}) #t)

  (test (run '{local {{datatype List
                                {Empty}
                                {Cons a b}}}
                {List? {Cons 1 2}}}) #t)

  (test (run '{local {{datatype List
                                {Empty}
                                {Cons a b}}}
                {Cons? {Cons 1 2}}}) #t)

  (test (run '{local {{datatype List
                                {Empty}
                                {Cons a b}}}
                {Empty? {Cons 1 2}}})
        #f)

  (test (run '{local {{datatype List
                                {Empty}
                                {Cons a b}}}
                {Empty? {Empty}}}) #t)

  (test (run '{local {{datatype List
                                {Empty}
                                {Cons a b}}}
                {Cons? {Empty}}})
        #f)

  ;; match
  (test (run '{match 1 {case 1 => 2}}) 2)

  (test (run '{match 2
                {case 1 => 2}
                {case 2 => 3}})
        3)

  (test (run '{match #t {case #t => 2}}) 2)

  (test (run '{match #f
                {case #t => 2}
                {case #f => 3}})
        3)

  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}})
        #t)
  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}}) #t))

;tests for extended MiniScheme+
;uncomment sanity tests when you are ready
#;(module+ sanity-tests
    (test (run '{local {{datatype Nat
                  {Zero}
                  {Succ n}}
                {define pred {fun {n}
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Zero}}}}} "ppwu") "{Succ {Zero}}")

(test (run
 `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 11 ones}}} "pp") "{list 1 1 1 1 1 1 1 1 1 1 1}")

(test (run `{local ,stream-lib
          {local {,stream-zipWith ,fibs}
            {stream-take 10 fibs}}} "pp") "{list 1 1 2 3 5 8 13 21 34 55}")

(test (run `{local ,stream-lib
          {local {,ones ,stream-zipWith}
            {stream-take 10
                         {stream-zipWith
                          {fun {n m}
                               {+ n m}}
                          ones
                          ones}}}} "pp")  "{list 2 2 2 2 2 2 2 2 2 2}")
(test
(run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,fibs ,stream-zipWith}
                 {stream-take 10 {merge-sort fibs fibs}}}} "pp")   "{list 1 1 1 1 2 2 3 3 5 5}"))

;WARM-UP

(test (pretty-printing (structV 'Nat 'Succ (list (structV 'Nat 'Zero empty))))      ;ejemplo enunciado
"{Succ {Zero}}")

(test (run '{local {{datatype Nat                                                    ;ejemplo enunciado
                  {Zero} 
                  {Succ n}}
                {define pred {fun {n} 
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Succ {Zero}}}}}} "ppwu")
"{Succ {Succ {Zero}}}")


(test (run '{local {{datatype List                             ;caso anidado
                        {Empty}
                        {Cons n1 rec}}}
       {Cons 33 {Cons 1 {Cons {Cons 4 5 } {Empty} }}}} "ppwu")  "{Cons 33 {Cons 1 {Cons {Cons 4 5} {Empty}}}}")


(test (run '{local {{datatype List
                        {Empty}
                        {Cons n1 rec}}}
       {Empty}} "ppwu") "{Empty}")


(test (run '{local {{datatype List
                        {Empty}
                        {Cons n1 rec}}   
                {define x {Empty}}}
               x}"ppwu" )  "{Empty}" )

(test (run '{local {{datatype List
                        {Empty}
                        {Cons n1 rec}}   
                {define x {Cons 1 2}}}
               x}"ppwu" )  "{Cons 1 2}" )


(test (run '{local {{datatype List
                        {Empty}
                        {Cons n1 rec}}   
                {define x {Cons {Cons 1 4} 2}}}
               x}"ppwu" )  "{Cons {Cons 1 4} 2}" )



(test (run '{local {{datatype T {C a  b}}    
                {define x {C  {/ 1 1}}}}
               x}"ppwu" )    "{C 1}")

(test (run '{Cons 1 2} "ppwu")"{Cons 1 2}")

(test (run '{local {{datatype List
                        {Empty}
                        {Cons n1 rec}}
              {define length {fun {n}
                                  {match n
                                    {case {Empty} => 0 }
                                    {case {Cons m1 m2 } => {+ 1 { length m2 } }}}}}}
       {length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}} "ppwu") 3)

(test (run '{+ 1 1} "ppwu") 2)
(test (run '{+ 1 1} "ppwu") 2)

(test (run '{Cons 1 {Cons 2 {Empty}}})  (structV 'List 'Cons (list 1 (structV 'List 'Cons (list 2 (structV 'List 'Empty '()))))))
(test (run '{Cons 1 {Cons 2 {Empty}}} "ppwu") "{Cons 1 {Cons 2 {Empty}}}")

(test (run '{Cons 1 {Cons {Cons 2 3 } {Empty}}}) (structV 'List 'Cons (list 1 (structV 'List 'Cons (list (structV 'List 'Cons '(2 3)) (structV 'List 'Empty '()))))))
(test (run '{Cons 1 {Cons {Cons 2 3 }}} "ppwu")  "{Cons 1 {Cons {Cons 2 3}}}" )


;LISTAS
;parte 1
(test (run '{Empty? {Empty}}) #t)                              ;ejemplo enunciado
(test (run '{Cons? {Empty}}) #f)
(test (run '{List? {Empty}}) #t)
(test (run '{List? {Cons 1 2}}) #t)                            ;ejemplo enunciado
(test (run '{Cons? {Cons 1 2}}) #t)
(test (run '{Empty? {Cons 1 2}}) #f)  
(test (run '{length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}) 3)    ;ejemplo enunciado
(test (run '{length {Empty}}) 0)
(test (run '{Cons 1 {Cons {Cons 1 2 } 4}} "ppwu") "{Cons 1 {Cons {Cons 1 2} 4}}")   ;anidada
(test  (run '{length {Cons {Cons 1 3} {Empty}}}) 1 )                                ;anidada
(test (run ' {Empty} "ppwu") "{Empty}")
(test (run ' {Empty} ) (structV 'List 'Empty '()))


;parte 2 (extender el lenguaje para soportar la notación '{list e1 e2 … en} )

(test (run '{Empty? {list 1 2}}) #f)
(test (run '{List? {list 1 2}}) #t)
(test (run '{List? {Cons 1 2}}) #t)
(test (run '{length {list 1 2 3}}) 3)
(test (run '{length {list 1 {list 12} 3 4}}) 4)    ;lista anidada

(test  (run '{match {list {+ 1 1} 4 6}   ;ejemplo enunciado
          {case {Cons h r} => h}
          {case _ => 0}})
       2)


(test (run '{match {list}                ;ejemplo enunciado
          {case {Cons h r} => h}
          {case _ => 0}})
      0)

;parte 3 (extender el lenguaje para soportar la notación '{list e1 e2 … en} para match)
(test (run '{match {list 2 {list 4 5} 6}           ;ejemplo enunciado
          {case {list a {list b c} d} => c}}) 5)


(test  (run '{match {list {+ 1 1} 4 6}   ;ejemplo enunciado
          {case {list a b c } => a}
          {case _ => 'f}})
       2)



;parte 4 (agregar flag “pp” para run que imprima las listas con el azúcar sintáctico de listas}.

(test (run '{length {list 1 2 3}} "pp") 3)
(test (run '{+ 1 1} "pp") 2)
(test (run '{list} "pp") "{list }")
(test (run '{list 1 2 } "pp" ) "{list 1 2 }")  
(test (run '{list 1 2 } "ppwu") "{Cons 1 {Cons 2 {Empty}}}")

(test (run '{list 1 {list 5} 6} )  (structV 'List 'Cons (list 1 (structV 'List 'Cons (list (structV 'List 'Cons (list 5 (structV 'List 'Empty '()))) (structV 'List 'Cons (list 6 (structV 'List 'Empty '()))))))))
(test (run '{list 1 {list 5} 6} "ppwu")  "{Cons 1 {Cons {Cons 5 {Empty}} {Cons 6 {Empty}}}}")
(test (run '{list 1 {list 5} 6} "pp")  "{list 1 {list 5 } 6 }")

(test (run '{local {{define x {list {list 1 4} 2}}} x}"pp" )  "{list {list 1 4 } 2 }" )
(test (run '{local {{define x {list}}} x}"pp" )  "{list }" )


;IMPLEMENTACION LAZY
(test (run '{{fun {x  {lazy y}}
                  x} 1 {/ 1 0}}) 1)
(test/exn  (run '{local {{define funcion {fun {x  {lazy y}}x} }
              {define x  {funcion 1 {/ 1 0}}}}
                x } ) "division by zero")

(test/exn (run '{{fun {x  y} x} 1 {/ 1 0}}) "division by zero")


(test (run '{local {{datatype T {C a {lazy b}}}
                {define x {C  0 {+ 1 2}}}}
               x} ) (structV 'T 'C (list 0 (prim-app '+ (list (num 1) (num 2))))))


(test (run '{local {{datatype T 
                  {C {lazy a}}}
                {define x {C {/ 1 0}}}}
          {T? x}}) #t)

(test (run '{local {{datatype T               ;caso lazy 
                              {C a {lazy b}}}
                    {define x {C  0 {/ 1 0}}}}
              x} ) (structV 'T 'C (list 0 (prim-app '/ (list (num 1) (num 0))))))

(test/exn (run '{local {{datatype T {C a  b}}    ;caso no lazy
                {define x {C  0 {/ 1 0}}}}
               x} )                            "division by zero")


(test (run '{local {{datatype T {C a  b}}    ;caso no lazy
                {define x {C  0 {/ 1 1}}}}
               x} )         (structV 'T 'C '(0 1)))                   

(test (run '{local {{datatype T {C a {lazy b}}}  ;caso no lazy bonito
                {define x {C  0 {/ 1 5 }}}}
               x} "pp")  "{ C 0 1/5  } "  ) ;ver este caso luego

(test/exn (run '{local {{datatype T {C a {lazy b}}}
              {define x {C  0 {/ 1 0 }}}}
               x} "pp")  "division by zero")
