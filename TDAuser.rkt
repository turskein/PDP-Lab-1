#lang racket
;Constructor TDA user
;dominio: string, string, date
;recorrido: user
(define (user name pass date)(
        list name pass date
))

(define (getName uss)
  (car uss)
  )

(define (eqUser? uss1 uss2)
  (if (string=? (getName uss1) (getName uss2))
      #t
      #f
    )
  )

(provide (all-defined-out))