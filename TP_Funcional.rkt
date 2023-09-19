(define momento-nuevo(lambda(d me a h min)
                (cons (list d me a)(list h min))
                       )
                 )
(define momento1 (momento-nuevo 14 12 2022 13 00))
(define momento2 (momento-nuevo 14 12 2022 15 30))
(define momento3 (momento-nuevo 14 12 2022 13 30))
(define momento4 (momento-nuevo 14 12 2022 14 00))
(define momento5 (momento-nuevo 14 12 2022 18 00))
(define momento6 (momento-nuevo 14 12 2022 16 30))
(define momento7 (momento-nuevo 15 12 2022 14 30))
(define momento8 (momento-nuevo 15 12 2022 22 10))
(define momento9 (momento-nuevo 23 12 2022 00 01))
(define momento10 (momento-nuevo 24 12 2022 23 59))


(define cita-nueva(lambda(tex ini fin)
                    (cons tex (cons ini (cons fin '())))
                    )
  
)
(define cita1 (cita-nueva "Odontologo de Joaquin" momento1 momento2))
(define cita2 (cita-nueva "Odontologo de Valen" momento3 momento4))
(define cita3 (cita-nueva "Odontologo de Pato" momento6 momento5))
(define cita4 (cita-nueva "Medico de Joaquin" momento6 momento5)) 
(define cita5 (cita-nueva "Medico de Valen" momento7 momento8))
(define cita6 (cita-nueva "Medico de Pato" momento1 momento2)) 
(define cita7 (cita-nueva "Navidad" momento9 momento10))

;Citas-Solapas recibe dos citas que esten en el mismo dia, devuelve true si estas se solapan, sino false.
(define citas-solapadas(lambda (cita1 cita2)
                         (if(equal? (car(cadr cita1)) (car(cadr cita2)))
                                (if (and(equal? (cadr(cadr cita1)) (cadr(cadr cita2)))
                                       (equal? (caddr(cadr cita1)) (caddr(cadr cita2))))
                                    #t
                                    (if (<(cadr(cadr cita1)) (cadr(caddr cita2)))
                                         (if(<(cadr(cadr cita2)) (cadr(caddr cita1)))
                                            #t
                                               (if(equal?(cadr(cadr cita2)) (cadr(caddr cita1)))
                                                    (if (<(caddr(cadr cita2)) (caddr(caddr cita1)))
                                                        #t
                                                        #f
                                                     )
                                                    #f
                                               )
                                          
                                          )
                                         (if(equal?(cadr(cadr cita1)) (cadr(caddr cita2)))
                                              (if (<(caddr(cadr cita1)) (caddr(caddr cita2)))
                                                  (if (equal?(cadr(cadr cita2)) (cadr(caddr cita1)))
                                                      (if(<(caddr(cadr cita2)) (caddr(caddr cita1)))
                                                           #t
                                                           #f
                                                   )
                                                  (if(<(cadr(cadr cita2)) (cadr(caddr cita1)))
                                                 #t
                                                 #f
                                                  )    
                                                  )
                                                  #f
                                               )
                                              
                                        #f )
                                   )
                                )
                                #f
                         )
                       )
)
;Recibe dos citas, si las mismas son identicas devuelve true, sino false.
(define citas-iguales (lambda (cita1 cita2)    
                        (if(equal? cita1 cita2)
                           #t
                           #f
                           )
                        )
  )

;Se recibe un nombre, el cual se creara un calendario nuevo con el nombre recibido como parametro.
(define calendario-nuevo (lambda (nombre)
                           (list nombre '())
                           )
  )

;Recibe un calendario definido, y un cita definida la cual se agregara al calendario.
(define calendario-agregar-cita (lambda (cal cita)
                                  (list (car cal) (cons cita (cadr cal)))
                                  )
  )

;Definicion de Calendarios                     
(define Calendario1 (calendario-agregar-cita (calendario-nuevo "Calendario-Joaquin") (list cita1 cita4)))
(define Calendario2 (calendario-agregar-cita (calendario-nuevo "Calendario-Valen")(list cita2 cita5)))
(define Calendario3 (calendario-agregar-cita (calendario-nuevo "Calendario-Pato") (list cita3 cita6)))
(define Calendario4 (calendario-agregar-cita (calendario-nuevo "Calendario-Navidad") (list cita7)))

;Aux utilizado en cita-mas-temprana.
(define Aux-elimina (lambda (elemento citas)
                      (if (null? citas) '()
                          (if (equal? elemento (car citas))
                              (Aux-elimina elemento (cdr citas))
                              (cons (car citas) (Aux-elimina elemento (cdr citas)))
                              )
                          )
                      )
)

;Define un calendario y una cita, la cual si esta ultima se encuentra dentro del calendario sera eliminada.
(define calendario-elimina-cita (lambda (cal cita)
                                  (if (null? (cdr cal)) cal
                                      (cons (car cal) (Aux-elimina cita (car(car(cdr cal)))))
                                      )
                                  )
)

;Aux de Calendarios-solapados?
(define Aux-CalendarioSolap (lambda (citaCal1 cal2)
                              (if (null? (cdr cal2))
                                  (citas-solapadas citaCal1 (car cal2))
                                  (if (citas-solapadas citaCal1 (car cal2)) #t
                                      (Aux-CalendarioSolap citaCal1 (cdr cal2))
                                      )
                                  )
                              )
  )

;Recibe dos calendarios, y verifica si alguna de sus citas se solapan.
(define calendarios-solapados? (lambda (cal1 cal2)
                                 (if (null? (cdr(car(car(cdr cal1)))))
                                 (if (null? (car(car(cdr cal1)))) #f
                                     (if (null? (car(car(cdr cal2)))) #f
                                         (Aux-CalendarioSolap (car(car(car(cdr cal1)))) (car(car(cdr cal2))))
                                         )
                                     )
                                 
                                  (Aux-CalendarioSolap (cadr(car(car(cdr cal1)))) (car(car(cdr cal2))))
                                  )
                                 )
)
;Esta funcion no la completamos al 100% porque no entendimos que seria el parametro pred, pero abajo
;definimos otra que busca la cita-mas-temprana dentro de un calendario.
(define encuentra-cita-mas-temprana (lambda(calen pred)
                                      (if (null? calen) 
                                          #f
                                          (if (pred calen (cita-mas-temprana (caar(cdr calen))))
                                              (cita-mas-temprana (caar(cdr calen)))
                                              (encuentra-cita-mas-temprana (calendario-elimina-cita calen (cita-mas-temprana ((caar(cdr calen)))) pred)
                                              )
                                          )
                                      )
  )
)

;Funcion que se le pasa un calendario y devuelve la cita mas temprana.
(define cita-mas-temprana (lambda (calendario)
                            (cita-mas-tempranaAUX (caar(cdr calendario)))                         
  )
  )
;Aux de cita-mas-temprana.
(define cita-mas-tempranaAUX (lambda(calen)
                            (if (null? (cdr calen))
                                (car calen)
                                (if (fecha-mas-proxima (car calen) (cita-mas-tempranaAUX (cdr calen))) 
                                    (car calen)
                                    (cita-mas-tempranaAUX (cdr calen))
                               )
                            )
                           )
)
;Función Auxiliar
;Devuelve True si la primer cita tiene fecha mas proxima, es decir, ocurre antes que cita 2. E.O.C retorna F                                
(define fecha-mas-proxima (lambda(c1 c2)
                            (if (< (caddr(cadr c1)) (caddr(cadr c2)))
                                #t
                                (if (and (eq? (caddr(cadr c1)) (caddr(cadr c2))) (< (cadr(cadr c1)) (cadr(cadr c2))))
                                    #t
                                    (if (and (eq? (caddr(cadr c1)) (caddr(cadr c2))) (eq? (cadr(cadr c1)) (cadr(cadr c2))) (< (caar (cadr c1)) (caar (cadr c2))))
                                        #t
                                        (if (and (eq? (caddr(cadr c1)) (caddr(cadr c2))) (eq? (cadr(cadr c1)) (cadr(cadr c2))) (eq? (caar (cadr c1)) (caar (cadr c2))) (< (cadr (cadr c1)) (cadr (cadr c2))))
                                            #t
                                            (if (and (eq? (caddr(cadr c1)) (caddr(cadr c2))) (eq? (cadr(cadr c1)) (cadr(cadr c2))) (eq? (caar (cadr c1)) (caar (cadr c2))) (eq? (cadr (cadr c1)) (cadr (cadr c2))) (< (caddr (cadr c1)) (caddr (cadr c2))))
                                                #t
                                                #f
                                             )
                                         )
                                     )
                                 )
                            )
                            )
  )

;Esta funcion no la completamos al 100% porque no entendimos que seria el parametro pred, pero abajo
;definimos otra que busca la cita-mas-tardia dentro de un calendario.
(define encuentra-cita-mas-tardia (lambda(calen pred)
                                     (if (null? calen) 
                                          #f
                                          (if (pred calen (cita-mas-tardia (caar(cdr calen))))
                                              (cita-mas-tardia (caar(cdr calen)))
                                              (encuentra-cita-mas-tardia (calendario-elimina-cita calen (cita-mas-tardia ((caar(cdr calen)))) pred)
                                              )
                                          )
                                      )
                                    )
)
;Funcion que se le pasa un calendario y devuelve la cita mas tardia.
(define cita-mas-tardia (lambda (calendario)
                            (cita-mas-tardiaAUX (caar(cdr calendario)))                         
  )
  )

;Aux de cita-mas-temprana.
(define cita-mas-tardiaAUX (lambda(calen)
                            (if (null? (cdr calen))
                                (car calen)
                                (if (fecha-mas-tardia (car calen) (cita-mas-tardiaAUX (cdr calen))) 
                                    (car calen)
                                    (cita-mas-tardiaAux (cdr calen))
                               )
                            )
                           )
)
;Función Auxiliar
;Devuelve True si la primer cita tiene fecha mas tardia, es decir, ocurre despues que cita 2. E.O.C retorna F
(define fecha-mas-tardia (lambda(c1 c2)
                            (if (> (caddr(cadr c1)) (caddr(cadr c2)))
                                #t
                                (if (and (eq? (caddr(cadr c1)) (caddr(cadr c2))) (> (cadr(cadr c1)) (cadr(cadr c2))))
                                    #t
                                    (if (and (eq? (caddr(cadr c1)) (caddr(cadr c2))) (eq? (cadr(cadr c1)) (cadr(cadr c2))) (> (caar (cadr c1)) (caar (cadr c2))))
                                        #t
                                        (if (and (eq? (caddr(cadr c1)) (caddr(cadr c2))) (eq? (cadr(cadr c1)) (cadr(cadr c2))) (eq? (caar (cadr c1)) (caar (cadr c2))) (> (cadr (cadr c1)) (cadr (cadr c2))))
                                            #t
                                            (if (and (eq? (caddr(cadr c1)) (caddr(cadr c2))) (eq? (cadr(cadr c1)) (cadr(cadr c2))) (eq? (caar (cadr c1)) (caar (cadr c2))) (eq? (cadr (cadr c1)) (cadr (cadr c2))) (> (caddr (cadr c1)) (caddr (cadr c2))))
                                                #t
                                                #f
                                             )
                                         )
                                     )
                                 )
                            )
                            )
)

;Recibe un calendario y lo muestra
(define calendario-muestra (lambda(calen)
                             (display (car calen))
                             (newline)
                             (mostrar-citas (caar(cdr calen)))
                             )
  )
;Función Auxiliar, muestra por pantalla las listas del calendario pasado como parámetro.
(define mostrar-citas (lambda (calen)
                        (if (null? (cdr calen))
                            (cita-muestra (car calen))
                            (cita-muestra (cita-mas-tempranaAUX calen))
                        )
                        (if (not(null?(Aux-elimina (cita-mas-tempranaAUX calen) calen)))
                            (mostrar-citas (Aux-elimina (cita-mas-tempranaAUX calen) calen))
                           )
                        )
  )
                            
                                    
                                    
;Función Auxiliar
(define cita-muestra (lambda(cita)
                       (display (car cita))
                       (newline)
                       (Aux-display-lista (momento->string-iso8601 (cadr cita)))
                       (Aux-display-lista (momento->string-iso8601 (caddr cita)))
                       )
  )
;Función Auxiliar
(define momento->string-iso8601 (lambda(momento)
                                  (list (caddr(car momento)) '- (cadr(car momento)) '- (caar momento) 'T (cadr momento) ': (caddr momento))
                                  )
  )
;Función Auxiliar
(define Aux-display-lista (lambda(lista)
                            (if (null?  lista) (newline)
                                (display (car lista))
                            )
                            (if (not (null? lista))
                            (Aux-display-lista (cdr lista))
                                )
                            )
  )
                                      
                                             