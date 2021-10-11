#lang racket
(require "TDAuser.rkt")
(require "TDAfecha.rkt")
(require "TDAversion.rkt")

;descripción: constructor TDA user de formato (nombre, idDocs, propietario , date, versions, ,access)
;dominio: string, date, string
;recorrido: docs
(define (docs name owner idDocs date)
  (list name owner idDocs date '() '())
  )

;descripción: se retorna el nombre del docs
;dominio: docs
;recorrido: string
(define (docs-name dcs)
  (list-ref dcs 0)
  )

;descripción: se retorna el nombre del propietario del docs
;dominio: docs
;recorrido: string
(define (docs-owner dcs)
  (list-ref dcs 1)
  )

;descripción: se retorna el id del docs
;dominio: docs
;recorrido: int
(define (docs-id dcs)
  (list-ref dcs 2)
  )
;descripción: se retorna el id del docs
;dominio: docs
;recorrido: date
(define (docs-date dcs)
  (list-ref dcs 3)
  )

;descripción: se retorna la lista de contenidos del docs
;dominio: docs
;recorrido: list
(define (docs-versions dcs)
  (list-ref dcs 4)
  )

;descripción: se retorna la lista de access que se tiene al documento
;dominio: docs
;recorrido: list
(define (docs-access dcs)
  (list-ref dcs 5)
  )
;descripción: se retorna el docs pero con la sección de versiones actualizada
;dominio: docs, list
;recorrido: docs
(define (docs-setversions dcs newversion)
  (list (docs-name dcs) (docs-owner dcs) (docs-id dcs) (docs-date dcs) newversion (docs-access dcs))
  )

;descripción: se retorna el docs pero con la sección de access actualizada
;dominio: docs, list
;recorrido: docs
(define (docs-setaccess dcs newaccess)
  (list (docs-name dcs) (docs-owner dcs) (docs-id dcs) (docs-date dcs) (docs-versions dcs) newaccess)
  )

;descripción: verifica si el id ingresado coincide con el del documento
;dominio: docs, int
;recorrido: boolean
(define (docs-rightid? dcs id)
  (= (docs-id dcs) id)
  )

;descripción: cuestiona si el usuario ingresado es el propietario del cocumento
;dominio: docs, user
;recorrido: boolean
(define (isowner? dcs user)
  (eq? (docs-owner dcs) (user-name user) )
  )

;descripción: agrega una nueva versión al documento generando una versión y apilandola a la anterior, o bien recién creandola
;dominio: docs, string
;recorrido: docs
(define (addnewversion dcs content)
  (if(eq? (docs-versions dcs) null)
  (docs-setversions dcs (cons (version content 0) (docs-versions dcs)))
  (docs-setversions dcs (cons (addcontent (car (docs-versions dcs)) content) (docs-versions dcs)))
  )
  )

;descripción: agrega un nuevo access al documento generandolo y apilandola a la anterior
;dominio: docs, access
;recorrido: docs
(define (addaccess dcs newaccess)
  (docs-setaccess dcs (cons newaccess (docs-access dcs)))
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