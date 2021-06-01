#lang play
;TEST PARTE 1

(require "t1.rkt")
(test (Expr? (num 5))true)
(test (Expr? (id 'x))true)
(test (Expr? (bool true))true)
(test (Expr? (bool false))true)
(test (Expr? (binop 'add 6 8))true)
(test (Expr? (binop 'sub 6 8))true)
(test (Expr? (unop 'sub1 8))true)
(test (Expr? (if true 4 5))true)
(test (Expr? (with '(((id 'x) (num 5)) ((id 'y)(num 7)) ((id 'z)(num 42))) (id 'z)) )true)
(test (Expr? (app (id 'f)'((num 3) (num 4)) ))true)
