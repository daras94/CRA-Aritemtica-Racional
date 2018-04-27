; Booleanos. Son los únicos lambda términos no currificados.

(define true (lambda (x y) x))

(define false (lambda (x y) y))

(define neg (lambda (x) (x false true)))

(define and (lambda (x y) (x y false)))

(define or (lambda (x y) (x true y)))

; Pares ordenados

(define par (lambda (x)
              (lambda (y)
                (lambda (f) (f x y)))))

(define primero (lambda (p) (p true)))

(define segundo (lambda (p) (p false)))

;;;;; Combinador de punto fijo

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;;;;;; Orden en naturales y test de nulidad

(define esmenoroigualnat (lambda (n)
                             (lambda (m)
                                (escero ((restanat n) m)))))

(define esmayoroigualnat (lambda (n)
                            (lambda (m)
                               (escero ((restanat m) n)))))

(define esmenornat (lambda (n)
                     (lambda (m)
                       (and ((esmenoroigualnat n) m) (noescero ((restanat m) n))))))

(define esmayornat (lambda (n)
                     (lambda (m)
                       (and ((esmayoroigualnat n) m) (noescero ((restanat n) m))))))

(define esigualnat (lambda (n)
                     (lambda (m)
                       (and ((esmayoroigualnat n) m) ((esmenoroigualnat n) m)))))

(define escero (lambda (n)
                 ((n (lambda (x) false)) true)))

(define noescero (lambda (n)
                    (neg (escero n))))

; Aritmética natural. Se define también "comprobar" para poder hacer pruebas. Se definen algunos naturales para hacer comprobaciones. Se escriben en francés para distinguirlos de los enteros
; que se escribirán en español.

(define zero (lambda (f)
               (lambda (x) x)))

(define sucesor (lambda (n)
                  (lambda (f)
                    (lambda (x)
                     (f((n f) x))))))

(define un (sucesor zero))

(define deux (sucesor un))

(define trois (sucesor deux))

(define quatre (sucesor trois))

(define cinq (sucesor quatre))

(define six (sucesor cinq))

(define sept (sucesor six))

(define huit (sucesor sept))

(define neuf (sucesor huit))

(define dix (sucesor neuf))

(define onze (sucesor dix))

(define douze (sucesor onze))

(define treize (sucesor douze))

(define quatorze (sucesor treize))

(define quinze (sucesor quatorze))

(define seize (sucesor quinze))

(define dix-sept (sucesor seize))

(define dix-huit (sucesor dix-sept))

(define dix-neuf (sucesor dix-huit))

(define vingt (sucesor dix-neuf))

;; Comprobar

(define comprobar (lambda (n)
                    ((n (lambda (x) (+ 1 x))) 0)))

;; Suma naturales

(define sumnat (lambda (n)
                 (lambda (m)
                   ((n (lambda (x) (sucesor x))) m))))

;; Producto naturales

(define prodnat (lambda (n)
                   (lambda (m)
                     (lambda (f)
                       (lambda (x) ((m (n f)) x))))))

(define prefn (lambda (f)
                (lambda (p)
                  ((par (f (primero p))) (primero p)))))

;; Predecesor y resta

(define predecesor (lambda (n)
                     (lambda (f)
                       (lambda (x)
                            (segundo ((n ((lambda (g)
                                             (lambda (p) ((prefn g) p))) f)) ((par x) x)))))))

(define restanat (lambda (n)
                     (lambda (m)
                        ((m (lambda (x) (predecesor x))) n))))

;; Resto de la división euclídea. Si el divisor es cero, devuelve false.

(define restonataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                 (lambda (x)
                    ((((esmayoroigualnat x) m)
                        (lambda (no_use)
                            (f ((restanat x) m))
                        )
                        (lambda (no_use)
                            x
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
))

(define restonat
  (lambda (n)
    (lambda (m)
      (((escero m) (lambda (no_use) false) (lambda (no_use) ((restonataux n) m))) zero)
    )
  )
)

;; Cociente de la división euclídea. Al igual que el resto, devuelve false si se divide por cero.

(define cocientenataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                (lambda (x)
                    ((((esmayoroigualnat x) m)
                        (lambda (no_use)
                            (sucesor (f ((restanat x) m)))
                        )
                        (lambda (no_use)
                            zero
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
    )
)

(define cocientenat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((cocientenataux n) m))) zero))))

;; Máximo común denominador.

(define mcdnat
    (lambda (n)
        (lambda (m)
            (((Y (lambda (f)
                   (lambda (x)
                     (lambda(y)
                      (((escero y)
                       (lambda (no_use)
                            x
                        )
                       (lambda (no_use)
                            ((f y)((restonat x) y))
                        )

                    )
                        zero)    ; Pasa zero como argumento de no_use
                ))
            ))
                n) ; Pasa n como el valor inicial de x.
          m)       ; Pasa m como el valor inicial de y.
    )
))

;;;; Paridad

(define par? (lambda (n)
               (escero ((restonat n) deux))))

(define cuadrado (lambda (n)
                   ((prodnat n) n)))


;;;;; Potencias de naturales usando algo binario.

(define potencianat
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                (lambda (y)
                    (((escero y)
                        (lambda (no_use)
                            un
                        )
                        (lambda (no_use)
                          (((par? y)
                           (lambda (no_use1)
                             (cuadrado (f ((cocientenat y) deux))))
                           (lambda (no_use1)
                             ((prodnat n) (f (predecesor y))))) zero)
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                m)  ; Pasa n como el valor inicial de x.
        )
    )
)

;;;;;; Definición de algunos enteros. Se codifican los enteros mediante pares de naturales: el par (m,n) es una representación de m-n. Es obvio que varios
;;;;;; pares codifican el mismo entero. Por ejemplo, (7,5)=(9,7). Por lo tanto, los enteros se definen como el conjunto cociente de NxN mediante la relación
;;;;;; de equivalencia R dada por
;;;;;;
;;;;;;                     (m,n) R (m',n') si y solo si m-n=m'-n'

(define cero ((par zero) zero))

(define -uno ((par zero) un))

(define -dos ((par zero) deux))

(define -tres ((par zero) trois))

(define -cuatro ((par zero) quatre))

(define -cinco ((par zero) cinq))

(define -seis ((par zero) six))

(define -siete ((par zero) sept))

(define -ocho ((par zero) huit))

(define -nueve ((par zero) neuf))

(define -diez ((par zero) dix))

(define -once ((par zero) onze))

(define -doce ((par zero) douze))

(define -trece ((par zero) treize))

(define -catorce ((par zero) quatorze))

(define -quince ((par zero) quinze))

(define -dieciseis ((par zero) seize))

(define -diecisiete ((par zero) dix-sept))

(define -dieciocho ((par zero) dix-huit))

(define -diecinueve ((par zero) dix-neuf))

(define -veinte ((par zero) vingt))

(define uno ((par un) zero))

(define dos ((par deux) zero))

(define tres ((par trois) zero))

(define cuatro ((par quatre) zero))

(define cinco ((par cinq) zero))

(define seis ((par six) zero))

(define siete ((par sept) zero))

(define ocho ((par huit) zero))

(define nueve ((par neuf) zero))

(define diez ((par dix) zero))

(define once ((par onze) zero))

(define doce ((par douze) zero))

(define trece ((par treize) zero))

(define catorce ((par quatorze) zero))

(define quince ((par quinze) zero))

(define dieciseis ((par seize) zero))

(define diecisiete ((par dix-sept) zero))

(define dieciocho ((par dix-huit) zero))

(define diecinueve ((par dix-neuf) zero))

(define veinte ((par vingt) zero))

;;;;; Orden, valor absoluto y tests de nulidad, positividad y negatividad.
;;;
;;; m-n > m'-n' si y solo si m+n' > m'+n e igual con el resto

(define esmayoroigualent (lambda (r)
                           (lambda (s)
                             ((esmayoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmenoroigualent (lambda (r)
                           (lambda (s)
                             ((esmenoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmayorent (lambda (r)
                           (lambda (s)
                             ((esmayornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmenorent (lambda (r)
                           (lambda (s)
                             ((esmenornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esigualent (lambda (r)
                           (lambda (s)
                             ((esigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define absoluto (lambda (r)
                    (((esmayoroigualnat (primero r)) (segundo r)) ((par ((restanat (primero r)) (segundo r))) zero) ((par ((restanat (segundo r)) (primero r))) zero))))

(define negativo (lambda (r)
                   ((esmenorent r) cero)))

(define positivo (lambda (r)
                   ((esmayorent r) cero)))

(define esceroent (lambda (r)
                     ((esigualnat (primero r)) (segundo r))))

(define noesceroent (lambda (r)
                       (neg (esceroent r))))

;;;;; Reducción a representante canónico de la clase de equivalencia.

(define reducir (lambda (r)
                  (((esmayoroigualnat (primero r)) (segundo r))
                        ((par ((restanat (primero r)) (segundo r))) zero)
                        ((par zero) ((restanat (segundo r)) (primero r))))))

;;;;; Aritmética entera. La respuesta está siempre dada por el representante canónico de la clase de equivalencia.

(define testenteros (lambda (r)
                      (- (comprobar (primero r)) (comprobar (segundo r)))))

(define sument (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat (primero r)) (primero s))) ((sumnat (segundo r)) (segundo s)))))))

(define prodent (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat ((prodnat (primero r)) (primero s))) ((prodnat (segundo r)) (segundo s))))
                          ((sumnat ((prodnat (primero r)) (segundo s))) ((prodnat (segundo r)) (primero s))))))))

(define restaent (lambda (r)
                   (lambda (s)
                     (reducir ((par ((sumnat (primero r)) (segundo s))) ((sumnat (segundo r)) (primero s)))))))

(define opuesto (lambda (r)
                  ((par (segundo r)) (primero r))))

;; Lo siguiente reduce la división de enteros a división de naturales. Si m mayor o igual que 0 y n > 0, y si q y r son cociente y resto de la división de m entre n, se tiene
;;  m  = q       * n        + r
;;  m  = (-q)    * (-n)     + r
;; -m  = (-(q+1))* n        + (n-r)
;; -m  = (q+1)   * (-n)     + (n-r),
;; siempre y cuando el resto no sea cero. Cuando el divisor es cero, la función cocienteent devuelve false.

(define cocienteent_aux (lambda (r)
                          (lambda (s)
                            ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))

; Caso1: resto cero. Si m= q*n, entonces -m= (-q)*n, -m = q* (-n) y m= (-q)*(-n).

(define cocienteentaux-caso1 (lambda (r)
                               (lambda (s)
                                  ((or (and ((esmayoroigualent r) cero) (positivo s)) (and (negativo r) (negativo s))) ((par ((cocientenat (primero (absoluto r))) (primero (absoluto s)))) zero)
                                                                                                                       ((par zero) ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))))

; Caso 2: resto no nulo

(define cocienteentaux-caso2 (lambda (r)
                                (lambda (s)
                                    (((esmayoroigualent r) cero) ((positivo s) ((par ((cocienteent_aux r) s)) zero) ((par zero) ((cocienteent_aux r) s)))
                                                                 ((positivo s) ((par zero) (sucesor ((cocienteent_aux r) s))) ((par (sucesor ((cocienteent_aux r) s))) zero))))))
; Cociente cuando no hay división por cero

(define cocienteentaux (lambda (r)
                         (lambda (s)
                           ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) ((cocienteentaux-caso1 r) s) ((cocienteentaux-caso2 r) s)))))

; Cociente considerando la división por cero

(define cocienteent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((cocienteentaux r) s))) zero))))

; Resto. Si se divide por cero, devuelve false

(define restoentaux1 (lambda (r)
                        (lambda (s)
                          ((or (and ((esmayoroigualent r) cero) (positivo s)) (and ((esmayoroigualent r) cero) (negativo s))) ((par ((restonat (primero (absoluto r))) (primero (absoluto s)))) zero)
                                                                                                           ((par ((restanat (primero (absoluto s)))((restonat (primero (absoluto r))) (primero (absoluto s))))) zero)))))

(define restoentaux (lambda (r)
                       (lambda (s)
                          ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) cero ((restoentaux1 r) s)))))

(define restoent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((restoentaux r) s))) zero))))

;; Como mcd (r,s)=mcd(|r|,|s|), se tiene

(define mcdent (lambda (r)
                 (lambda (s)
                   ((par ((mcdnat (primero (absoluto r))) (primero (absoluto s)))) zero))))

;; Mínimo común múltiplo

(define mcment (lambda (r)
                 (lambda (s)
                   ((cocienteent ((prodent r) s)) ((mcdent r) s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aritmetica RACIONALES (Parte 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test de racionales
;; TEST:              > (test_racionales ((par -diez) doce))
;; Resultado          > (-10 12)

  (define test_racionales (lambda (r) (list (testenteros (primero r)) (testenteros (segundo r)))))

; a1) Reducción a representante canónico.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reducción RACIONALES:
;; TEST:              > (test_racionales (reducir_racionales ((par diez) doce)))
;; Resultado          > (5 6)

  (define reducir_racionales
    (lambda (a)
      ((or (and (positivo (primero a)) (positivo (segundo a))) (and (negativo (primero a)) (negativo (segundo a))))
        ((par ((cocienteent (absoluto (primero a))) ((mcdent (primero a)) (segundo a)))) ((cocienteent (absoluto (segundo a))) ((mcdent (primero a)) (segundo a))))
          ((par (opuesto ((cocienteent (absoluto (primero a))) ((mcdent (primero a)) (segundo a))))) ((cocienteent (absoluto (segundo a))) ((mcdent (primero a)) (segundo a)))))
    )
  )

; b1) Aritmética: suma, producto, resta de racionales y cálculo de inverso.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suma racionales
;; TEST 1:            > (test_racionales ((suma_racionales ((par diez) doce)) ((par -diez) doce)))
;; Resultado test 1   > (0 1)
;; TEST 2:            > (test_racionales ((suma_racionales ((par trece) doce)) ((par -diez) siete)))
;; Resultado test 2   > (-29 84)

  (define suma_racionales
    (lambda (a)
      (lambda (b)
          (reducir_racionales
            ((par ((sument ((prodent (primero a)) (segundo b))) ((prodent (primero b)) (segundo a)))) ((prodent (segundo a)) (segundo b)))
          )
      )
    )
  )

;; Resta racionales
;; TEST 1:            > (test_racionales ((resta_racionales ((par trece) doce)) ((par -diez) siete)))
;; Resultado test 1   > (211 84)
;; TEST 2:            > (test_racionales ((resta_racionales ((par diez) doce)) ((par -diez) doce)))
;; Resultado test 2   > (5 3)

  (define resta_racionales
    (lambda (a)
      (lambda (b)
          (reducir_racionales
            ((par ((restaent ((prodent (primero a)) (segundo b))) ((prodent (primero b)) (segundo a)))) ((prodent (segundo a)) (segundo b)))
          )
      )
    )
  )

 ;; Producto racionales
 ;; TEST 1:            > (test_racionales ((producto_racionales ((par trece) doce)) ((par diez) siete)))
 ;; Resultado test 1   > (65 42)
 ;; TEST 2:            > (test_racionales ((producto_racionales ((par diez) doce)) ((par -diez) doce)))
 ;; Resultado test 2   > (-25 36)

  (define producto_racionales
   (lambda (a)
      (lambda (b)
        (reducir_racionales
            ((par ((prodent (primero a)) (primero b))) ((prodent (segundo a)) (segundo b)))
        )
      )
   )
  )

 ;; Inverso racionales
 ;; TEST 1:            > (inverso_racionales ((par cero) uno))
 ;; Resultado test 1   > #<procedure:false>
 ;; TEST 2:            > (test_racionales (inverso_racionales ((par diez) doce)))
 ;; Resultado test 2   > (6 5)

  (define inverso_racionales
   (lambda (a)
      (((esceroent (primero a))
        (lambda (no_use) false)
        (lambda (no_use)
          (reducir_racionales
            ((par ((prodent (segundo a)) uno)) ((prodent (primero a)) uno))
          )
        )
      ) zero)
   )
  )

 ; c1) Relaciones de orden e igualdad.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; >= racionales
 ;; TEST 1:            > ((esmayoroigual_racional ((par diez) -doce)) ((par diez) cuatro))
 ;; Resultado test 1   > #<procedure:true>
 ;; TEST 2:            > ((esmayoroigual_racional ((par -diez) dos)) ((par diez) cuatro))
 ;; Resultado test 2   > #<procedure:false>
 ;; TEST 3:            > ((esmayoroigual_racional ((par diez) dos)) ((par diez) dos))
 ;; Resultado test 3   > #<procedure:true>

  (define esmayoroigual_racional
   (lambda (r)
     (lambda (s)
        ((esmayoroigualent ((prodent (primero r)) (segundo s))) ((prodent (segundo r)) (primero s)))
     )
   )
  )

;; <= racionales
;; TEST 1:            > ((esmenoroigual_racional ((par diez) doce)) ((par diez) cuatro))
;; Resultado test 1   > #<procedure:true>
;; TEST 2:            > ((esmenoroigual_racional ((par diez) dos)) ((par diez) cuatro))
;; Resultado test 2   > #<procedure:false>
;; TEST 3:            > ((esmenoroigual_racional ((par diez) dos)) ((par diez) dos))
;; Resultado test 3   > #<procedure:true>

  (define esmenoroigual_racional
   (lambda (r)
      (lambda (s)
        ((esmenoroigualent ((prodent (primero r)) (segundo s))) ((prodent (segundo r)) (primero s)))
      )
   )
  )

;; > racionales
;; TEST 1:            > ((esmayor_racional ((par diez) doce)) ((par diez) cuatro))
;; Resultado test 1   > #<procedure:false>
;; TEST 2:            > ((esmayor_racional ((par diez) dos)) ((par diez) cuatro))
;; Resultado test 2   > #<procedure:true>


  (define esmayor_racional
    (lambda (r)
       (lambda (s)
         ((esmayorent ((prodent (primero r)) (segundo s))) ((prodent (segundo r)) (primero s)))
       )
    )
  )

;; < racionales
;; TEST 1:            > ((esmenor_racional ((par diez) doce)) ((par diez) cuatro))
;; Resultado test 1   > #<procedure:true>
;; TEST 2:            > ((esmenor_racional ((par diez) dos)) ((par diez) cuatro))
;; Resultado test 2   > #<procedure:false>

  (define esmenor_racional
    (lambda (r)
       (lambda (s)
         ((esmenorent ((prodent (primero r)) (segundo s))) ((prodent (segundo r)) (primero s)))
       )
    )
  )

;; == racionales
;; TEST 1:            > ((esigual_racional ((par diez) -dos)) ((par diez) dos))
;; Resultado test 1   > #<procedure:false>
;; TEST 2:            > ((esigual_racional ((par diez) dos)) ((par diez) dos))
;; Resultado test 2   > #<procedure:true>

  (define esigual_racional
    (lambda (r)
       (lambda (s)
          ((esigualent ((prodent (primero r)) (segundo s))) ((prodent (segundo r)) (primero s)))
       )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aritmetica RACIONALES Matrices (Parte 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Matriz de prueba racionales
;; TEST 1:            > (test_racionales (primero (primero matriz_prueba1)))
;; Resultado test 1   > (1 2)
;; TEST 2:            > (test_racionales (primero (segundo matriz_prueba1)))
;; Resultado test 2   > (1 5)
;; TEST 1:            > (test_racionales (segundo (primero matriz_prueba1)))
;; Resultado test 1   > (1 1)
;; TEST 2:            > (test_racionales (segundo (segundo matriz_prueba1)))
;; Resultado test 2   > (-1 1)

  (define matriz_prueba1 ((par ((par ((par uno) dos))   ((par uno) uno)))
                               ((par ((par uno) cinco)) ((par -uno) uno))))

  (define matriz_prueba2 ((par ((par ((par cinco) dos))   ((par uno) dos)))
                               ((par ((par ocho) -cinco)) ((par -uno) uno))))

 ;; test de matrizes racionales
 ;; TEST 1:           > (test_matriz matriz_prueba1)
 ;; Resultado test 1  > (((1 2) (1 5)) ((1 1) (-1 1)))
 ;; TEST 2:           > (test_matriz matriz_prueba2)
 ;; Resultado test 2  > (((5 2) (8 -5)) ((1 2) (-1 1)))

   (define test_matriz
     (lambda (r)
       (list (list (test_racionales (primero (primero r))) (test_racionales (primero (segundo r))))
             (list (test_racionales (segundo (primero r))) (test_racionales (segundo (segundo r)))))
     )
   )

; a2) Suma y producto.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suma de matrices racionales
;; TEST 1:            > (test_matriz ((suma_matriz matriz_prueba1) matriz_prueba2))
;; Resultado test 1   > (((3 1) (3 2)) ((-7 5) (-2 1)))

  (define suma_matriz
    (lambda (a)
      (lambda (b)
        ((par ((par ((suma_racionales (primero (primero a))) (primero (primero b)))) ((suma_racionales (primero (segundo a))) (primero (segundo b)))))
              ((par ((suma_racionales (segundo (primero a))) (segundo (primero b)))) ((suma_racionales (segundo (segundo a))) (segundo (segundo b)))))
      )
    )
  )

;; producto de matrices racionales
;; TEST 1:            > (test_matriz ((producto_matriz matriz_prueba1) matriz_prueba2))
;; Resultado test 1   > (((27 20) (-1 1)) ((2 1) (-3 5)))
;; TEST 2:            > (test_matriz ((producto_matriz matriz_prueba2) matriz_prueba1))
;; Resultado test 2   > (((-7 20) (21 10)) ((-3 4) (11 10)))

  (define producto_matriz
    (lambda (a)
      (lambda (b)
        ((par ((par ((suma_racionales ((producto_racionales (primero (primero a))) (primero (primero b)))) ((producto_racionales (primero (segundo a))) (segundo (primero b)))))
                    ((suma_racionales ((producto_racionales (segundo (primero a))) (primero (primero b)))) ((producto_racionales (segundo (segundo a))) (segundo (primero b))))))
              ((par ((suma_racionales ((producto_racionales (primero (primero a))) (primero (segundo b)))) ((producto_racionales (primero (segundo a))) (segundo (segundo b)))))
                    ((suma_racionales ((producto_racionales (segundo (primero a))) (primero (segundo b)))) ((producto_racionales (segundo (segundo a))) (segundo (segundo b))))))
      )
    )
  )

; b2) Determinante de matrizes racionales
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; producto de matrices racionales
;; TEST 1:            > (test_racionales (determinante matriz_prueba1))
;; Resultado test 1   > (-7 10)
;; TEST 2:            > (test_racionales (determinante matriz_prueba2))
;; Resultado test 2   > (-17 10)

  (define determinante
    (lambda (a)
      ((resta_racionales ((producto_racionales (primero (primero a))) (segundo (segundo a)))) ((producto_racionales (primero (segundo a))) (segundo (primero a))))
    )
  )

; c2) Decisión sobre inversibilidad y cálculo de inversa y del rango
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Matrizes transpuesta racionale
;; TEST 1:            > (test_matriz (matriz_transpuesta matriz_prueba1))
;; Resultado test 1   > (((-1 1) (1 5)) ((1 1) (1 2)))
;; TEST 2:            > (test_matriz (matriz_transpuesta matriz_prueba2))
;; Resultado test 2   > (((-1 1) (8 -5)) ((1 2) (5 2)))

  (define matriz_transpuesta
    (lambda (a)
      ((par ((par (segundo (segundo a))) (segundo (primero a))))  ; Hay que cocegir hacer el negativo de (segundo (primero a))
            ((par (primero (segundo a))) (primero (primero a))))  ; Hay que cocegir hacer el negativo de (primero (segundo a))
    )
  )

  ;; Condicion de inversivilidad
  ;; TEST 1:            > (esinversible_matriz matriz_prueba1)
  ;; Resultado test 1   >
  ;; TEST 2:            > (esinversible_matriz matriz_prueba2)
  ;; Resultado test 2   >

  (define esinversible_matriz  ; acaba debido a que no hace el negativo.
    (lambda (a)
      ((esigual_racional (determinante ((producto_matriz a) (matriz_transpuesta a)))) (determinante ((producto_matriz (matriz_transpuesta a)) a)))
    )
  )
