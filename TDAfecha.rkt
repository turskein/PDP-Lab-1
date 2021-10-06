#lang racket
;Descripción: Constructor de TDA fecha
;dominio: int, int , int
;recorrido: list
(define (date dia mes año)(
       if(and (integer? dia) (integer? mes) (integer? año))
       (if(and (and (> dia 0) (< dia 32)) (and (> mes 0) (< mes 13)) (> año 0))
            (list dia mes año)
            null
            )
       null
    )
)
;Descripción: Pertenencia TDA fecha
;dominio: list
;recorrido: boolean
(define (date? date)(
if (eq? date empty)
  #f
  (if(and (integer? (car date)) (integer? (car (cdr date))) (integer? (car (cdr (cdr date)))))
       (if(and (and (> (car date) 0) (< (car date) 32)) (and (> (car (cdr date)) 0) (< (car (cdr date)) 13)) (> (car (cdr (cdr date))) 0))
            #t
            #f
            )
       #f
    )
)
)
;Descripción: Modificadores
;dominio: int, list
;recorrido: list

(define (setDay newDay date)(
   if (and (> newDay 0) (< newDay 32))
      (list newDay (car (cdr date)) (car (cdr (cdr date))))
      null
  )
)

(define (setMonth newMonth date)(
   if (and (> newMonth 0)( < newMonth 13))
      (list (car date) newMonth (car (cdr (cdr date))))
      null
  )
)

(define (setYear newYear date)(
    if(> newYear 0)
      (list (car date) (car (cdr date)) newYear)
      null
  )
)

;Descripción: Selectores
;dominio: list
;recorrido: int
;Seleccionar Dia
(define (getDay date)(
    if(date? date)
      (list (car date))
      null
  )
)
;Seleccionar Mes
(define (getMonth date)(
    if(date? date)
      (list (car (cdr date)))
      null
  )
)

;Seleccionar anio
(define (getYear date)(
     if(date? date)
      (list (car (cdr (cdr date))))
      null
  )
)

;
;Ejemplos....................
;

;Ejemplo constructor TDA date
(date 2 5 6)
(date 5 6 9)
(date 2 5 -2)

;Ejemplo Pertenencia
(date? (date 2 5 6))
(date? (date 3 6 8))
(date? (date 32 3 4))

;Ejemplo Modificadores
;Modificar dia
(setDay 3 (date 2 5 6))
(setDay 16 (date 2 5 6))
(setDay 32 (date 2 5 6))
;Modificar mes
(setMonth 3 (date 2 5 6))
(setMonth 9 (date 2 5 6))
(setMonth 13 (date 2 5 6))
;Modificar anio
(setYear 3 (date 2 5 6))
(setYear 2000 (date 2 5 6))
(setYear 0 (date 2 5 6))

;Ejemplo Selectores
;Seleccionar dia
(getDay (date 2 5 6))
(getDay (date 2 10 2000))
(getDay '(0 3 2000))
;Seleccionar mes
(getMonth (date 2 5 6))
(getMonth (date 2 10 2000))
(getMonth '(0 3 2000))
;Seleccionar anio
(getYear (date 2 5 6))
(getYear (date 2 10 2000))
(getYear '(0 3 2000))