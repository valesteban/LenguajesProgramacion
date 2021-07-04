#lang play
(require "version2.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
              (seqn {+ x 1}
                    x))) 5)

;; Ejemplos del enunciado
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-x (+ 1 3))
             (+ (send o sum 3) (send o get-y)))))
      11)

(test (run-val
       '(local
            [(define a
               (object
                (method auto-apply (o)
                        (send o apply o))
                (method foo () 5)
                ))
             (define o (send a auto-apply
                             (object
                              (method apply (other) (send other apply2 this))
                              (method apply2 (other) this)
                              (method foo () 42))))]
          (send o foo)))
      42)

#;(test (run-val '(local
              [(define smart-computer (object
                                       (method secret? (something) 42)))
               (define everything (object))
               (define oracle (object : smart-computer))]
               (send oracle secret? everything)))
      42)

#;(run-val '(local
              [(define seller (object
                               (method multiplier () 1)
                               (method price (item-number)
                                       (* item-number (send this multiplier)))))
               (define broker (object : seller
                                      (method multiplier () 2)))]
               (send broker price 3)))

#;(test (run-val '(local
                    ([define x (object
                                (field z 3)
                                (method get () (get z)))]
                     [define y (object : x)])
                  (send y get)))
      3)

#;(test/exn (run-val '(local
                        ([define x (object
                                    (field z 3)
                                    (method get () (get z)))]
                         [define y (object
                                    : x
                                    (method get () (get z)))])
                      (send y get)))
          "field not found")

;; A simple monotone counter
(define counter '(object
                  (field count 0)
                  (method incr () (set count (+ 1 (get count))))
                  (method get () (get count))))

(define (incrs-multiply x y)
  `(seqn
    (send ,y incr)
    (seqn
     (send ,x incr)
     (seqn
      (send ,x incr)
      (* (send ,x get) (send ,y get))
      ))))

#;(test (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

#;(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      16)

#;(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (deep-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Se crea el objeto y se ocupa un metodo dentro de el (metodo que no necesita fields de el mismo o las expr get o set)

(test (run-val '(local
              [(define o (object
                          (field x [+ 2 1])
                          (field y 2)
                          (method sum (a) (+ 100 a))
                          (method get-y () (get y))))]
           (send o sum 55 )))  155)
;objeto son ni fields ni metodos
(test  (run-val '(local
              [(define o (object
                        ))]
           {+ 100 40}))  140)



(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (a) (+ (get x) a))))]
           (send o sum 55 ))) 56)

;lo mismo que lo anterior pero con dos objetos donde sus metodos y lo demas son todo igual excepto el nombre por el cual defino el objeto

(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (a) (+ (get x) a))))
               
               (define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (a) (+ (get x) a))))]
           (send o sum 55 ))) 56)


;se pueden ver entre ellos 
(test (run-val '(local
              [(define o1 (object
                          (field z 4)
                          (field w 5)
                          (method amb () (+ 100 1))))
               
               (define o2 (object
                          (field x 1)
                          (field y 2)
                          (method otro () {send o1 amb})))]
           {send o2 otro})) 101)


(test (run-val '(local
   [(define o (object
               (field x 1)
               (field y 2)
               (method sum (a) (+ 100 a))
               (method get-y () (get y))))
    (define o2 (object
               (field x 1)
               (field y 2)
               (method sum (b) (+ 100 b))
               (method get-y () (get y))))]
   
   (send o2 sum 55 ))) 155)

;llamar a una funcion de un objeto

(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (a) (+ 100 a))
                          (method get-y () (get y))))]
           (send o get-y )))  2)

;llamar una funcion de un objeto que usa get

(test(run-val '(local
              [(define o (object
                          (field x 10)
                          (field y 2)
                           (method sum (z) (+ (get x) (+ (get y) z)))
                          (method get-y () (get y))))]
           (send o sum 400))) 412)


;llama a una fncion se mutea su valor y despues se ocupa una un metodo del objeto

(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-x  3)
             (send o sum 3) ))) 8)
