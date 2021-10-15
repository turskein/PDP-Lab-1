#lang racket

;descripción: constructor de TDA access compuesto por: nombre de usuario y tipo de acceso al documento
;dominio: string, char
;recorrido: access
(define (access nameuser kind)
  (list nameuser kind)
  )

;descripción: retorna el nombre del usuario del acceso
;dominio: access
;recorrido: string(nombre de usuario)
(define (access-user acc)
  (list-ref acc 0)
  )

;descripción: retorna el tipo de acceso
;dominio: access
;recorrido: character
(define (access-kind acc)
  (list-ref acc 1)
  )

(provide (all-defined-out))