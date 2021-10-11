#lang racket

;descripción: constructor TDA versión de la forma: (contendio , id)
;dominio: string, int
;recorrido: version
(define (version content id)
  (list content id)
  )

;descripción: se obtiene el contenido de la versión
;dominio: versión
;recorrido: string
(define (version-content vrsn)
  (car vrsn)
  )

;descripción: se obtiene el id de la versión
;dominio: versión
;recorrido: integer
(define (version-id vrsn)
  (car (cdr vrsn))
  )

;descripción: se agrega el contenido nuevo a la el contenido de una versión anterior, agregando en uno el ID
;dominio: versión, string
;recorrido: versión
(define (addcontent lastvrsn newcontent)
  (version (string-append (version-content lastvrsn) newcontent) (+ 1 (version-id lastvrsn)))
  )

(provide (all-defined-out))