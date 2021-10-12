#lang racket

;descripción: constructor de TDA access compuesto por: nombre de usuario y tipo de acceso al documento
;dominio: string, char
;recorrido: access

(define (access nameuser kind)
  (list nameuser kind)
  )

(define (access-user acc)
  (list-ref acc 0)
  )

(define (access-kind acc)
  (list-ref acc 1)
  )

  (provide (all-defined-out))