#lang racket
(require "TDAuser.rkt")
(require "TDAfecha.rkt")

;descripción: constructor TDA user de formato (nombre, idDocs, propietario , date, contents, ,access)
;dominio: string, date, string
;recorrido: docs
(define (docs name owner idDocs date)
  (list name owner idDocs date '() '())
  )
;descripción: se retorna el id del docs
;dominio: docs
;recorrido: int
(define (docs-idDocs dcs)
  (list-ref dcs 2)
  )

;descripción: se retorna el usuario del propietario del docs
;dominio: docs
;recorrido: string
(define (docs-owner dcs)
  (list-ref dcs 1)
  )

;descripción: se retorna la lista de contenidos del docs
;dominio: docs
;recorrido: list
(define (docs-contents dcs)
  (list-ref dcs 4)
  )

;descripción: se retorna la lista de access que se tiene al documento
;dominio: docs
;recorrido: list
(define (docs-access dcs)
  (list-ref dcs 5)
  )

;descripción: cuestiona si el usuario ingresado es el propietario del cocumento
;dominio: docs, user
;recorrido: boolean
(define (isOwner? dcs user)
  (eq? (docs-owner dcs) (user-name user) )
  )
;descripción: cuestiona si el usuario ingresado puede escribir en el cocumento
;dominio: docs, user
;recorrido: boolean
;(define (canwrite? dcs user)())

;descripción: cuestiona si el usuario ingresado puede escribir en el cocumento
;dominio: docs, user
;recorrido: boolean
;(define (cancomment? dcs user))

(provide (all-defined-out))