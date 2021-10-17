#lang racket
(require "TDAuser.rkt")
(require "TDAfecha.rkt")
(require "TDAversion.rkt")
(require "TDAaccess.rkt")

;descripción: constructor TDA user de formato (nombre, idDocs, propietario , date, versions, access)
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
;dominio: docs, string(name user)
;recorrido: boolean
(define (isowner? dcs user)
  (eq? (docs-owner dcs) user )
  )

;descripción: ingresa una nueva versión a la lista de versiones existente
;dominio: docs, string(name), date
;recorrido: docs
(define (addnewversion dcs content date)
  (if(eq? (docs-versions dcs) null)
     (docs-setversions dcs (cons (addcontent (version "" -1 date) content date) (docs-versions dcs)))
     (docs-setversions dcs (cons (addcontent (version "" (version-id (car (docs-versions dcs))) date) content date) (docs-versions dcs)))
     )
  )

;descripción: agrega una nueva versión al documento generando una versión y apilandola a la anterior, o bien recién creandola
;dominio: docs, string
;recorrido: docs
(define (addnewversionwithlast dcs content date)
  (docs-setversions dcs (cons (addcontent (car (docs-versions dcs)) content date) (docs-versions dcs)))
  )
;descripción: verifica si el nombre del acceso en la entrada ya existe dentro de la lista de accesos al documento
;dominio: lista de accesos, access
;recorrido: booleano
;recursividad: natural
(define existaccess
  (lambda (listaccess acs)
    (if (eq? listaccess null)
        #f
        (if(eq? (access-user (car listaccess)) (access-user acs) )
           #t
           (existaccess (cdr listaccess) acs)
           )
        )
    )
  )
;descripción: agrega un nuevo access al documento y si existe lo actualiza a uno nuevo
;dominio: docs, access
;recorrido: docs
(define (addaccess dcs newaccess)
  (if(existaccess (docs-access dcs) newaccess)
     (docs-setaccess dcs (map (lambda (acs)
                                (if(eq? (access-user acs) (access-user newaccess))
                                   newaccess
                                   acs
                                   )
                                ) (docs-access dcs)))
     (docs-setaccess dcs (cons newaccess (docs-access dcs)))
     )
  )

;descripción: cuestiona si el usuario ingresado puede escribir en el cocumento
;dominio: docs, string(name user)
;recorrido: boolean
;recursividad: natural
(define (canwrite? dcs user)
  (define (can? listaccess user)
    (if(eq? listaccess null)
       #f
       (if(string=? (access-user (car listaccess)) user)
          #t
          (can? (cdr listaccess) user)
          )
       )
    )
  (can? (docs-access dcs) user)
  )

;descripción: retorna una versión en particular del documento
;dominio: docs, int(id de versión)
;recorrido: versión
;recursividad: cola
(define (docs-getsomeversion dcs id)
  (if(eq? (version-id (car(docs-versions dcs))) id)
     (car(docs-versions dcs))
     (docs-getsomeversion (docs-setversions dcs (cdr (docs-versions dcs))) id)
     )
  )
;descripción: elimina todos los accesos del documento
;dominio: docs
;recorrido: docs
(define (docs-kickall dcs)
  (docs-setaccess dcs '())
  )

;descripción: elimina todos los accesos de un documento
;dominio: docs
;recorrido: docs
;(define (docs-revoke))

;descripción: cuestiona si el usuario ingresado puede escribir en el cocumento
;dominio: docs, user
;recorrido: boolean
;(define (cancomment? dcs user))

(provide (all-defined-out))