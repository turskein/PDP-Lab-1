#lang racket
(require "TDAuser.rkt")
(require "TDAfecha.rkt")
(require "TDAversion.rkt")
(require "TDAaccess.rkt")
(require "EncryptFn_DencryptFn.rkt")

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

(define (existcontentindoc? doc searchText)
  (if (eq? (docs-versions doc) null)
      #f
      (if (existsubstring? searchText (version-content (car (docs-versions doc))))
         #t
         (existcontentindoc? (docs-setversions doc (cdr (docs-versions doc))) searchText)
         )
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

;descripción: cuestiona si el usuario ingresado puede leer el cocumento
;dominio: docs, string(nombre de usuario)
;recorrido: boolean
;recursividad: cola
(define (canread? dcs user)
  (if(or (isowner? dcs user) (canwrite? dcs user))
     #t
     (if(eq? (docs-access dcs) null)
        #f
        (if(and (string=? (access-user (car (docs-access dcs))) user) (eq? (access-kind (car (docs-access dcs)))) #\r)
           #t
           (canread? (docs-setaccess dcs (cdr(docs-access dcs) )) user)
           )
        )
     )
  )
;descripción: se mostrará strings donde se señala el usuario y el tipo de accesos que tiene
;dominio: doc
;recorrido: string
;recursión: natural
(define (docs-displayaccess doc)
     (if(eq? (docs-access doc) null)
        ""
        (string-append "usuario: " (access-user (car (docs-access doc))) "\n" "tipo de acceso: " (if (eq? (access-kind (car(docs-access doc))) #\r)
                                                                                                   "Lectura"
                                                                                                   (if (eq? (access-kind (car(docs-access doc))) #\w)
                                                                                                       "Escritura"
                                                                                                       (if (eq? (access-kind (car(docs-access doc))) #\c)
                                                                                                           "Comentar"
                                                                                                           "Propietario"
                                                                                                           )
                                                                                                       )
                                                                                                   ) "\n" (docs-displayaccess (docs-setaccess doc (cdr (docs-access doc)))))
        )
     )

;descripción: se mostrará la información correspondiente a un documento de acuerdo a si el usuario ingresado es o no el owner
;dominio: doc, string(nombre de usuario), operation(función que desencriptará el contenido), int(si se ingresa un número se ejecutará una sección distinta del código)
;recorrido: string
;recursión: natural
(define (docs-readable doc user decrypter [option 0])
  (if(= option 0)
     (if(and (canread? doc user) (not (eq? (docs-versions doc) null)))
        (if(isowner? doc user)
           (string-append "Nombre documento: " (docs-name doc) "\n" "Id-doc: "  (number->string(docs-id doc)) "\n" "Fecha de creación: " (date->string (docs-date doc)) "\n" "----Accesos---- " "\n" (docs-displayaccess doc) "\n" "---Versiones--- " "\n" "versión: " (number->string (version-id (car (docs-versions doc)))) "\n" "Fecha versión: " (date->string (version-date (car (docs-versions doc)))) "\n" "Contenido: "(decrypter (version-content (car (docs-versions doc)))) "\n" (docs-readable (docs-setversions doc (cdr (docs-versions doc))) user decrypter 1))
           (string-append "Nombre documento: " (docs-name doc) "\n" "versión : " (number->string (version-id (car (docs-versions doc)))) "\n" "Fecha versión: " (date->string (version-date (car (docs-versions doc)))) "\n" "Contenido: "(decrypter (version-content (car (docs-versions doc))))  "\n")
           )
        ""
        )
     (if(eq? (docs-versions doc) null)
        ""
        (string-append "Versión: " (number->string (version-id (car (docs-versions doc)))) "\n" "Fecha versión: " (date->string (version-date (car (docs-versions doc)))) "\n" "Contenido: " (decrypter (version-content (car (docs-versions doc)))) "\n" (docs-readable (docs-setversions doc (cdr (docs-versions doc))) user decrypter 1))
        )
     )
  )
;descripción: busca en una cadena de texto si existe otra cadena de texto
;dominio: string(palabra a buscar), string(palabra en la que se buscará), int(opcional, inicio de la palabra que se buscará),int(opcional, largo de la palabra que se buscará)
;recorrido: boolean
;recursión: cola
(define (existsubstring? lfword lfinword [init 0] [end (string-length lfword)])
    (if(> end (string-length lfinword))
       #f
       (if(string-ci=? (substring lfinword init end) lfword)
          #t
          (existsubstring? lfword lfinword (+ init 1) (+ end 1))
          )
       )
    )

(provide (all-defined-out))