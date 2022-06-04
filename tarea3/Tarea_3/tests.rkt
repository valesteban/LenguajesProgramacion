#lang play
(require "main.rkt")
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

(test (run-val '(local
              [(define smart-computer (object
                                       (method secret? (something) 42)))
               (define everything (object))
               (define oracle (object : smart-computer))]
               (send oracle secret? everything)))
      42)

(test (run-val '(local
              [(define seller (object
                               (method multiplier () 1)
                               (method price (item-number)
                                       (* item-number (send this multiplier)))))
               (define broker (object : seller
                                      (method multiplier () 2)))]
               (send broker price 3)))
      6)

(test (run-val '(local
                    ([define x (object
                                (field z 3)
                                (method get () (get z)))]
                     [define y (object : x)])
                  (send y get)))
      3)

(test/exn (run-val '(local
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

(test (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

(test (run-val
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
;COSAS EXTRAS SOBRE MI IMPLEMENTACION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;- los fields no se evaluan hasta el final , si es que se les necesita
;- cada objeto se guarda a si mismo en su env y si esque tiene a su padre
;- en el (send obj1 met) se ocupa el env de obj1
;- para la herencia se hace que un objeto se guarde a si mismo en su ambiente y tambien a su padre
;- solo para los casos excepcionales de get  y set no se ocupa el ambinete del q llamo sino del de donde esta el metodo



;test simples
(test (run-val '(local
                  [(define o (object
                              (field x [+ 2 1])
                              (field y 2)
                              (method sum (a) (+ 100 a))
                              (method get-y () (get y))))]
                  (send o sum 55 )))  155)
;test con un objeto vacio
(test  (run-val '(local
                   [(define o (object ))]
           {+ 100 40}))  140)

;test no se encuentra un metodo
(test/exn  (run-val '(local
                   [(define o (object ))]
           {send o metodo})) "method not found")

;test no encuentra un field
(test/exn  (run-val '(local
                   [(define o (object
                               (field z 1)
                               (method get () (get x))
                               ))]
           {send o get})) "field not found")

;test ocupando this en una parte nada que ver

;test ocupando this en una parte nada que ver
(test/exn (run-val '(local
                   [(define o (object
                               (field z 1)
                               (method get () (get x))
                               ))]
           this)) "this used outside of an object")

;test ocupando set fuera de un objeto
(test/exn (run-val '(local
                   [(define o (object
                               (field z 1)
                               (method get () (get x))
                               ))]
           (get z))) "get used outside of an object")

;test ocupando set fuera de un objetos
(test/exn (run-val '(local
                   [(define o (object
                               (field z 1)
                               (method get () (get x))
                               ))]
           (set z 10))) "set used outside of an object")


;test ocupando this en una parte nada que ver
(test/exn (run-val '(local
                   [(define o (object
                               (field z 1)
                               (method set (val) (set z val))
                               (method get () (get x))
                               ))]
           (seqn
            (send o set 44)
            this))) "this used outside of an object")

;LLAMADOS A LOS FIELDS
;test se evalua lo que halla en el field a mostrar
(test (run-val '(local
              [(define o (object
                          (field x [+ 2 1])
                          (field y this)
                          (field z [get x])
                          (method get-x () (get x))
                          (method get-z () (get z))
                          (method get-y () (get y))))]
           (send o get-x ))) 3)

;test field anidado dentro de otro field
(test (run-val '(local
              [(define o (object
                          (field x [+ 2 1])
                          (field y this)
                          (field z [get x])
                          (method get-x () (get x))
                          (method get-z () (get z))
                          (method get-y () (get y))))]
           (send o get-z ))) 3)

;test setear un valor y devolverlo, uso seqn
(test (run-val '(local ([define o (object
                                   (field y 10)
                                   (field x (get y))
                                   (method set-y (a)(set y a))
                                   (method get-x () (get x)))])
                  (seqn
                   (send o set-y 4)
                   (send o get-x)))) 4)

;test setear un valor y devolverlo pero anidado
(test (run-val '(local ([define o (object
                                   (field y 10)
                                   (field x (get y))
                                   (method set-555 ()(set y 555))
                                   (method get-x () (get x)))])
                  (seqn
                   (send o set-555 )
                   (send o get-x )))) 555)

;test get dentro de un metodo con set
(test (run-val '(local ([define c (object
                                   (field count 0)
                                   (method incr () (set count (+ 1 (get count))))
                                   (method get () (get count)))])
                  (seqn
                   (send c incr)
                   (send c get))))  1)


;test con mas objetos 

;test con dos objetos muy parecidos 
(test (run-val '(local
              [(define o2 (object
                          (field x 100)
                          (field y 2)
                          (method sum (a) (+ (get x) a))))
               (define o1 (object
                          (field x 200)
                          (field y 2)
                          (method sum (a) (+ (get x) a))))]
           (send o2 sum 55 ))) 155)


;test donde objetos se "ven" entre ellos 
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

;test llamar a los fields de un objeto
(test (run-val '(local
              [(define o (object
                          (field x 10)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method get-y () (get y))))]
           (send o sum 400))) 412)

;test lo mismo que lo qbteriror pero antes  se setea un field
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

;test ocupando this dentro de un field 
(test (run-val'(local
             [(define o (object
                         (field z 100)
                         (field x 2)
                         (field y (send this get-x))
                         (method get-x () (get x))
                         (method get-y () (get y))
                         (method set-x (v) (set x v))))]             
             (seqn
              (send o set-x 10)
              (send o get-y)))) 10)

;test con this
(test (run-val '(local [(define o (object
                             (field a this)
                             (field b 8)
                             (method get-a () (get a ))
                             (method cien () 100)))]
            (send (send o get-a) cien))) 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                   DELEGACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;delegacion simple
(test (run-val '(local
            ([define x (object
                        (field f 3)
                        (method mil () 1000 )
                        (method sum () (+ 1 2)))]
             [define y (object : x  
                        (field f 5)
                        (method sum () (+ 2 3 ))
                        (method cien () 100))])
            (send y mil))) 1000)

(test (run-val '(local
            [(define seller (object
                             (method multiplier () 1)
                             (method price (item-number)
                                     (* item-number (send this multiplier)))))
             (define broker (object : seller
                                    (method multiplier () 2)))]
            (send broker price 3))) 6)

;test se setea el  field de x (con delegacion) y por lo tanto al pedir z del objeto x vemos que este cambio
;delegacion  *2
(test (run-val '(local
                  ([define x (object
                              (field z 0)
                              (method get-z () (get z))
                              (method 100 () 100)
                              (method set-z (a) (set z a)))]
                   [define y (object : x
                              (field z 2)
                              (method get-z () (get z)))]
                   [define z (object : y
                              (field m 0)
                              (method get-m ()(get m)))])
                  (seqn
                   (send z set-z 5)
                   (send x get-z)))) 5)

;test no se encuentra un metodo en ninguno de los objetos
(test/exn (run-val '(local
             ([define x (object
                         (field z 3)
                         (method get () (get z)))]
              [define y (object : x
                                (method get () (get z)))])
                      (send y get))) "field not found")

;test fiel no encontrado
(test/exn (run-val '(local
            ([define x (object
                        (field z 3)
                        (method get () (get z)))]
             [define y (object : x
                        (method get () (get z)))])
            (send y get)))
          "field not found")

;test con this 
(test (run-val '(local [(define o (object
                             (field a this)
                             (field b 8)
                             (method get-a () (get a ))
                             (method cien () 100)))
                  (define o2 (object : o
                              (field f 5)
                              (method get-f () (get f ))))]
            (send (send o2 get-a) cien))) 100)

;CASO DE THIS CON HERENCIA
;test devolver this dentro de un field devuelve x
(test (run-val '(local   
                    ([define x (object
                                (field z this)
                                (method get () (get z)))]
                     [define y (object : x)])
                  (send y get)))
      "(object padre: NO  fields: (#(struct:field z #&#(struct:this yo))) metodos: (#(struct:method get () #(struct:get z))) noquiero ver elambiente")

;test devolver this  devuelve y
(test (run-val '(local 
                    ([define x (object
                                (field z this)
                                (method get-this () this)
                                (method get-z () (get z)))]
                     [define y (object : x)])
                  (send y get-this)))
      "(object padre: #(struct:id x)  fields: () metodos: () noquiero ver elambiente")


;test se setea un field de x y por lo tanto al pedir z del objeto este sigue igual
(test (run-val '(local 
                    ([define x (object
                                (field z 0)
                                (method get-z () (get z))
                                (method set-z (a) (set z a)))]
                     [define y (object : x
                                (field z 2)
                                (method get-z () (get z)))])
                  (seqn
                   (send y set-z 44)
                   (send y get-z ))))
      2)

;test se le pasa al padre entero , no s ededfine antes
(test (run-val
       '(local ([define c (object : (object
                           (field count 0)
                           (method incr () (set count (+ 1 (get count))))
                           (method get () (get count))))])
          (send c get))) 0)

;test lo mismo anterior pero con dos pasos , usando seqn
(test (run-val
       '(local ([define c (object : (object
                           (field count 0)
                           (method incr () (set count (+ 1 (get count))))
                           (method get () (get count))))] )
          (seqn
           (send c incr)
           (send c get))))
      1)

;test muchos metodos anidados dentro de otros y get 
(test (run-val '(local
            ([define x (object
                        (field f 3)
                        (method mil () 1000 )
                        (method sum () (+ (send y sum) (get f) )))]
             [define y (object : x  
                        (field f 5)
                        (method sum () (+ (send y mil) (get f) ))
                        (method cien () 100))])
            (send x sum))) 1008)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            SHALLOW COPY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;solo me salieron las del enunciado  
(test (run-val
       '(local ([define c (object
                           (field count 0)
                           (method incr () (set count (+ 1 (get count))))
                           (method get () (get count)))])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  (send c2 get )))))
1)

(test (run-val
       '(local ([define c (object
                           (field count 0)
                           (method incr () (set count (+ 1 (get count))))
                           (method get () (get count)))])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  (seqn
                   (send c2 incr)
                   (send c2 get )))))) 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         DEEP-COPY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            F
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                      LAMBDAS CON OBJETOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(local
              [(define f (fun (x)
                              (+ x x)))]
              (f 5)))
      10)

(test (run-val '(local
              [(define f (fun ()
                              (or #t #f)))]
              (f )))
      #t)


(test (run-val '(local
              [(define f (fun (a b c )
                              (* a (+ b c))))]
              (f 1 2 3)))
5)

(test (run-val '(local
              [(define f (fun (a b c )
                              (* a (+ b c))))
               (define o (object
                          (field m 4)
                          (method get () (get m))))]
              (+ (send o get)(f 1 2 3)))) 9)

(test (run-val '(local
              [(define f (fun (a b c )
                              (* a (+ b c))))
               (define o (object
                          (field m 4)
                          (method get () (get m))
                          (method fun ( a b c) (send f fun a b c))))]
            (send o fun 1 2 3))) 5)


