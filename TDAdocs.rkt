#lang racket
(require "TDAuser.rkt")
(require "TDAfecha.rkt")
(require "TDAversion.rkt")
(require "TDAaccess.rkt")
(require "EncryptFn_DencryptFn.rkt")
(require "TDAcomentario.rkt")
;REPRESENTACIÓN
;string(nombre), int(idDoc), string(nombre del propietario) , date, list(lista de versions), list(lista de access), list(lista versiones)

;CONSTRUCTOR
;descripción: TDA docs de formato
;dominio: string, date, string
;recorrido: docs
(define (docs name owner idDocs date)
  (list name owner idDocs date '() '() '())
  )

;SELECTORES
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

(define (docs-memory dcs)
  (list-ref dcs 6)
  )

;descripción: retorna la última versión de un documento
;dominio: docs
;recorrido: version
(define (docs-lastversion dcs)
  (car (docs-versions dcs))
  )

;descripción: retorna la última versión existente en memory
;dominio: docs
;recorrido: versión
(define (docs-lastmemory dcs)
  (car (docs-memory dcs))
  )

;MODIFICADORES
;descripción: se retorna el docs pero con la sección de versiones actualizada
;dominio: docs, list
;recorrido: docs
(define (docs-setversions dcs newversion)
  (list (docs-name dcs) (docs-owner dcs) (docs-id dcs) (docs-date dcs) newversion (docs-access dcs) (docs-memory dcs))
  )

;descripción: se retorna el docs pero con la sección de access actualizada
;dominio: docs, list
;recorrido: docs
(define (docs-setaccess dcs newaccess)
  (list (docs-name dcs) (docs-owner dcs) (docs-id dcs) (docs-date dcs) (docs-versions dcs) newaccess (docs-memory dcs))
  )

(define (docs-setmemory dcs newmemory)
  (list (docs-name dcs) (docs-owner dcs) (docs-id dcs) (docs-date dcs) (docs-versions dcs) (docs-access dcs) newmemory)
  )

;-FUNCIONES ADICIONALES-

;FUNCIONES BOOLEANAS
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

;descripción: verifica en todas las versiones del documento si existe un contenido en específico, ya sea sub-string o no
;dominio: doc, string(texto buscado)
;recorrido: boolean
;recursividad: cola
(define (existcontentindoc? doc searchText)
  (if (eq? (docs-versions doc) null)
      #f
      (if (existsubstring? searchText (version-content (car (docs-versions doc))))
         #t
         (existcontentindoc? (docs-setversions doc (cdr (docs-versions doc))) searchText)
         )
      )
  )

;descripción: verifica si el nombre del acceso en la entrada ya existe dentro de la lista de accesos
;dominio: lista de accesos, access
;recorrido: booleano
;recursividad: cola
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

;descripción: cuestiona si el usuario ingresado puede escribir en el cocumento
;dominio: docs, string(name user)
;recorrido: boolean
;recursividad: cola
(define (canwrite? dcs user)
  (define (can? listaccess user)
    (if(eq? listaccess null)
       #f
       (if(string=? (access-user (car listaccess)) user)
          (if (eq? (access-kind (car listaccess)) #\w)
              #t
              #f
              )
          (can? (cdr listaccess) user)
          )
       )
    )
  (or (can? (docs-access dcs) user) (isowner? dcs user))
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
        (if(and (string=? (access-user (car (docs-access dcs))) user) (eq? (access-kind (car (docs-access dcs))) #\r))
           #t
           (canread? (docs-setaccess dcs (cdr(docs-access dcs) )) user)
           )
        )
     )
  )

;descripción: cuestiona si el usuario ingresado puede comentar el documento
;dominio: docs, string(nombre de usuario)
;recorrido: boolean
;recursividad: cola
(define (cancomment? dcs user)
  (define (can? listaccess user)
    (if(eq? listaccess null)
       #f
       (if(string=? (access-user (car listaccess)) user)
          (if (eq? (access-kind (car listaccess)) #\c)
              #t
              #f
              )
          (can? (cdr listaccess) user)
          )
       )
    )
  (or (can? (docs-access dcs) user) (isowner? dcs user) (canwrite? dcs user))
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

;descripción: agrega un nuevo access al documento y si existe lo actualiza a uno nuevo
;dominio: docs, access
;recorrido: docs
(define (addaccess dcs newaccess)
  (if(not(isowner? dcs (access-user newaccess)))
     (if(existaccess (docs-access dcs) newaccess)
        (docs-setaccess dcs (map (lambda (acs)
                                   (if(eq? (access-user acs) (access-user newaccess))
                                      newaccess
                                      acs
                                      )
                                   ) (docs-access dcs)))
        (docs-setaccess dcs (cons newaccess (docs-access dcs)))
        )
     dcs
     )
  )

;descripción: restaura la memoria de un documento a vacío
;dominio: doc
;recorrido: doc
(define (docs-restartmemory dcs)
  (docs-setmemory dcs null)
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
; FUNCIONES QUE MUESTRAN CONTENIDO
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
           (string-append "\n###Nombre documento: " (docs-name doc)"\n" "Propietario: " (docs-owner doc) "\n" "Id-doc: "  (number->string(docs-id doc)) "\n" "Fecha de creación: " (date->string (docs-date doc)) "\n" "----Accesos---- " "\n" (docs-displayaccess doc) "\n" "---Versiones--- " "\n" "-%Versión: " (number->string (version-id (docs-lastversion doc))) "\n" "Fecha versión: " (date->string (version-date (docs-lastversion doc))) "\n" "Contenido: "(decrypter (version-content (docs-lastversion doc))) "\n" "Comentarios: " (version-displaycomments (docs-lastversion doc) decrypter) "\n" (docs-readable (docs-setversions doc (cdr (docs-versions doc))) user decrypter 1))
           (string-append "\n###Nombre documento: " (docs-name doc) "\n" "Propietario: " (docs-owner doc) "-%Versión : " (number->string (version-id (docs-lastversion doc))) "\n" "Fecha versión: " (date->string (version-date (docs-lastversion doc))) "\n" "Contenido: "(decrypter (version-content (docs-lastversion doc)))  "\n")
           )
        ""
        )
     (if(eq? (docs-versions doc) null)
        ""
        (string-append "-%Versión: " (number->string (version-id (docs-lastversion doc))) "\n" "Fecha versión: " (date->string (version-date (docs-lastversion doc))) "\n" "Contenido: " (decrypter (version-content (docs-lastversion doc))) "\n" "Comentarios: " (version-displaycomments (docs-lastversion doc) decrypter) "\n" (docs-readable (docs-setversions doc (cdr (docs-versions doc))) user decrypter 1))
        )
     )
  )

;FUNCIONES QUE MODIFICAN VERSIONES
;descripción: ingresa una nueva versión a la lista de versiones existente, en caso de no existir una la genera con id 0, pero en caso de existir una requerirá la última versión para generar otra con el id en serie
;dominio: docs, string(contenido), date
;recorrido: docs
(define (addnewversion dcs content date)
  (if(eq? (docs-versions dcs) null)
     (docs-setversions dcs (cons (addcontent (version "" -1 date) content date) (docs-versions dcs)))
     (docs-setversions dcs (cons (addcontent (version "" (version-id (car (docs-versions dcs))) date) content date) (docs-versions dcs)))
     )
  )

;descripción: agrega una nueva versión al documento generando una versión y apilandola a la anterior, o bien recién creandola
;dominio: docs, string, date
;recorrido: docs
(define (addnewversionwithlast dcs content date)
  (docs-setversions dcs (cons (addcontent (car (docs-versions dcs)) content date) (docs-versions dcs)))
  )

;descripción: elimina una cantidad de caracteres de la última versión del documento
;dominio: doc, int(cantidad de caracteres)
;recorrido: doc
(define (doc-deletchars dcs number date encrypter decrypter)
  (if(> number (string-length (decrypter (version-content (docs-lastversion dcs)))))
     (addnewversion dcs "" date)
     (addnewversion dcs (encrypter (substring (decrypter (version-content(docs-lastversion dcs))) 0 (- (string-length (decrypter (version-content (docs-lastversion dcs)))) number))) date)
     )
  )
;descripción: reemplaza un texto buscado dentro de la última versión por un texto en particular
;dominio: docs, date, string(texto buscado), string(texto por el que reemplazar)
;recorrido: doc
(define (doc-searchreplace dcs date searchText replaceText)
  (addnewversion dcs (string-replace (version-content (docs-lastversion dcs)) searchText replaceText) date)
  )

;descripción: aplica estilos en particular una frase en específico
;dominio: doc, string(texto buscado), list(estilos), función(encriptador), función(desencriptador)
;recorrido: doc
(define (doc-applystyles date dcs searchText estilos encrypter decrypter)
  (addnewversion dcs (encrypter (string-replace (decrypter (version-content (docs-lastversion dcs))) searchText (string-append (string-join estilos) searchText (string-join estilos)))) date)
  )

;descripción: agrega un comentario relacionado a un texto seleccionado dentro de la versión actual
;dominio: doc, date, string(texto seleccionado), string(texto del comentario), función(encriptador), función(desencriptador)
;recorrido: docs
(define (doc-commentsometext dcs date selectedText commenText encrypter decrypter)
  (if (existsubstring? selectedText (decrypter (version-content (docs-lastversion dcs))))
      (docs-setversions dcs (cons (version-addcomment (docs-lastversion dcs) (comentario (encrypter selectedText) (encrypter commenText) date)) (cdr (docs-versions dcs))))
      dcs
      )
  )
;descripción: traspasa versiones de la sección versiones a memory una por una
;dominio: doc, int(cantidad de versiones a traspasar)
;recorrido: doc
;recursividad: cola
(define (doc-deshacer dcs numberOfUndo)
  (if (or (= (length (docs-versions dcs)) 0) (= numberOfUndo 0))
      dcs
      (doc-deshacer (docs-setmemory (docs-setversions dcs (cdr (docs-versions dcs))) (cons (docs-lastversion dcs) (docs-memory dcs))) (- numberOfUndo 1) )
      )
  )

;descripción: traspasa versiones de la sección memory a versiones una por una
;dominio: doc, int(cantidad de versiones a traspasar)
;recorrido: doc
;recursividad: cola
(define (doc-rehacer dcs numberOfRedo)
  (if (or (= (length (docs-memory dcs)) 0) (= numberOfRedo 0))
      dcs
      (doc-rehacer (docs-setmemory (docs-setversions dcs (cons (docs-lastmemory dcs) (docs-versions dcs))) (cdr (docs-memory dcs))) (- numberOfRedo 1))
      )
  )

(provide (all-defined-out))