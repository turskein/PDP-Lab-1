#lang racket

;descripción: Constructor TDA user
;dominio: string, string, date
;recorrido: user
(define (user name pass date)(
        list name pass date
))

;descripción: se retorna el nombre del usuario
;dominio: user
;recorrido: string
(define (user-name uss)
  (car uss)
  )

;descripción: se retona la constraseña del usuario
;dominio: user
;recorrido: string
(define (user-pass uss)
  (car (cdr uss))
  )

(define (user-date uss)
  (car (cdr (cdr uss)))
  )

;descripción: verifica si coinciden los nombres de usuario
;dominio: user, user
;recorrido: boolean
(define (eqUser? uss1 uss2)
  (string=? (user-name uss1) (user-name uss2))
  )

;descripción: verifica si coinciden los nombres de usuario
;dominio: user, user
;recorrido: boolean
(define (eqPass? uss1 uss2)
  (string=? (user-pass uss1) (user-pass uss2))
  )

(provide (all-defined-out))