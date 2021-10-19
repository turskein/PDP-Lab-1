#lang racket
;Descripción: Constructor de TDA fecha
;dominio: int, int , int
;recorrido: list
(define (date dia mes anio)(
       if(and (integer? dia) (integer? mes) (integer? anio))
       (if(and (and (> dia 0) (< dia 32)) (and (> mes 0) (< mes 13)) (< anio 2022))
            (list dia mes anio)
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
   if(eq? date null)
      null
   (if (and (> newDay 0) (< newDay 32))
      (list newDay (car (cdr date)) (car (cdr (cdr date))))
      null)
  )
)

(define (setMonth newMonth date)(
   if(eq? date null)
      null(
      if (and (> newMonth 0)( < newMonth 13))
        (list (car date) newMonth (car (cdr (cdr date))))
        null
  )
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
      (car date)
      -1
  )
)
;Seleccionar Mes
(define (getMonth date)(
    if(date? date)
      (car (cdr date))
      -1
  )
)

;Seleccionar anio
(define (getYear date)(
     if(date? date)
      (car (cdr (cdr date)))
      -1
  )
)

(define (date->string date)
  (string-append (number->string (getDay date))" "(number->string (getMonth date)) " " (number->string (getYear date)))
  )

(provide (all-defined-out))