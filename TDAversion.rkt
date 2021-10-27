#lang racket

;descripción: constructor TDA versión de la forma: (contendio , id)
;dominio: string, int, date
;recorrido: version
(define (version content id date)
  (list content id date)
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

;descripción: retorna la fecha de una versión
;dominio: versión
;recorrido: date
(define (version-date vrsn)
  (car (cdr (cdr vrsn)))
  )

;descripción: retorna el largo del contenido de una versión
;dominio: versión
;recorrido: int(largo del contenido de una versión)
(define (version-length vrsn)
  (string-length (version-content vrsn))
  )

;descripción: se agrega el contenido nuevo a la el contenido de una versión anterior, agregando en uno el ID
;dominio: versión (version anterior del documento), string (nuevo contenido), date
;recorrido: versión
(define (addcontent lastvrsn newcontent date)
  (version (string-append (version-content lastvrsn) newcontent) (+ 1 (version-id lastvrsn)) date)
  )

(provide (all-defined-out))