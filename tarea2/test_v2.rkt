#lang play

(require "main_v2.rkt")
;; Test sub-module.
;; See http://blog.racket-lang.org/2012/06/submodules.html

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

;LISTAS
;parte 1
(test (run '{Empty? {Empty}}) #t)                              ;ejemplo enunciado
(test (run '{List? {Cons 1 2}}) #t)                            ;ejemplo enunciado
(test (run '{length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}) 3)    ;ejemplo enunciado

 
