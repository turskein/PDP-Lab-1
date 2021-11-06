#lang racket
(require "TDAfecha.rkt")
(require "EncryptFn_DencryptFn.rkt")
(require "TDAuser.rkt")
(require "TDAversion.rkt")
(require "TDAdocs.rkt")
(require "TDAaccess.rkt")

;REPRESENTACIÓN
;string(nombre de la plataforma), date(fecha creación plataforma), función(función de encriptación), función(función de desencriptación), list(lista de usuarios registrado en la plataforma), list(lista de documentos creados en la plataforma)

;CONSTRUCTOR
;descripción: constructor de paradigmadocs
;dominio: str, date, EncryptFn, DencryptFn
;recorrido: paradigmadocs
(define paradigmadocs(lambda (name date encrypter decrypter)(
        list name date encrypter decrypter '() '() '()
    )
))

;SELECTORES
;descripción: selector de nombre de paradigmadocs
;dominio: paradigmadocs
;recorrido: string
(define prdoc-name(lambda (prdcs)(
        car prdcs
   )
  )
 )

;descripción: selector de fecha de paradigmadocs
;dominio: paradigmadocs
;recorrido: lista (fecha)
(define prdoc-date(lambda (prdcs)(
        list-ref prdcs 1
   )
  )
  )

;descripción: selector de nombre de paradigmadocs
;dominio: paradigmadocs
;recorrido: string
(define prdoc-encrypter(lambda (prdcs)(
        list-ref prdcs 2
   )
  )
 )

;descripción: selector de nombre de paradigmadocs
;dominio: paradigmadocs
;recorrido: string
(define prdoc-decrypter(lambda (prdcs)(
        list-ref prdcs 3
   )
  )
 )

(define prdoc-sesion(lambda (prdcs)(
        list-ref prdcs 4
   )
  )
 )

;descripción: agregar un usuario a lista de usuarios en paradigmadocs
;dominio: paradigmadocs y usuario a agregar
;recorrido: paradigmadocs
(define prdoc-users(lambda (prdcs)(
    list-ref prdcs 5
)))
;descripción: selector de documentos de paradigmadocs
;dominio: paradigmadocs
;recorrido: lista de documentos
(define (prdoc-docs prdcs)(
        list-ref prdcs 6
   )
  )

;MODIFICADORES

;descripción: actualizar lista de usuarios registrados en paradigmadocs
;dominio: paradigmadocs, lista
;recorrido: paradigmadocs
(define (prdoc-setregisters prdcs newregisters)
  (list (prdoc-name prdcs) (prdoc-date prdcs) (prdoc-encrypter prdcs) (prdoc-decrypter prdcs) (prdoc-sesion prdcs) newregisters (prdoc-docs prdcs))
  )

;descripción: actualizar lista de sesiones en paradigmadocs
;dominio: paradigmadocs, lista
;recorrido: paradigmadocs
(define (prdoc-setsesion prdcs newsesion)
  (list (prdoc-name prdcs) (prdoc-date prdcs) (prdoc-encrypter prdcs) (prdoc-decrypter prdcs) newsesion (prdoc-users prdcs) (prdoc-docs prdcs))
  )
;descripción: actualizar lista de documentos en paradigmadocs
;dominio: paradigmadocs, lista
;recorrido: paradigmadocs
(define (prdoc-setdocs prdcs newdocs)
  (list (prdoc-name prdcs) (prdoc-date prdcs) (prdoc-encrypter prdcs) (prdoc-decrypter prdcs) (prdoc-sesion prdcs) (prdoc-users prdcs) newdocs)
  )

;-FUNCIONES ADICIONALES-
;FUNCIONES BOOLEANAS

;descripción: verifica si hay algún usuario en la paradigmadocs
;dominio: paradigmadocs
;recorrido: boolean
(define (prdoc-thereusers? prdcs)
  (not (null? (prdoc-users prdcs)))
  )

;descripción: verifica la existencia de un usuario con su nombre de usuario y su contraseña encriptada
;dominio: lista users, user
;recorrido: boolean
(define (rightuserpass? users usr)
     (if(null? users)
        #f
        (if(and (eqUser? (car users) usr) (eqPass? (car users) usr) )
           #t
           (rightuserpass? (cdr users) usr)
        )
     )
  )

;descripción: verifica si existe algún usuario activo
;dominio: paradigmadocs
;recorrido: boolean
(define (prdoc-theresesion? prdcs)
  (not (eq? (prdoc-sesion prdcs) null))
  )

;FUNCIONES QUE MODIFICAN

;descripción: agrega un nuevo usuario al final de la lista, verificando que el usuario no exista dentro de paradgimadocs
;dominio: paradigmadocs, user, option(verifica si se encontro o no un usuario)
;recorrido: paradigmadocs
;recursión: natural
(define (prdoc-adduser users usr [opt 0])
  (if(not (null? users))
     (if (eqUser? (car users) usr)
         (cons (car users) (prdoc-adduser (cdr users) usr 1))
         (cons (car users) (prdoc-adduser (cdr users) usr opt))
         )
     (if (= opt 1)
         null
         (list usr)
         )
     )
  )

;descripción: agrega un usuario a la sesión activa
;dominio: paradigmadocs, string: nombre de usuario
;recorrido: paradigmadocs
(define (prdoc-addactiveuser prdcs username)
  (prdoc-setsesion prdcs (cons username (prdoc-sesion prdcs)))
  )

;descripción: agregar un nuevo documento a la plataforma paradigmadocs
;dominio: paradigmadocs, docs
;recorrido: paradigmadocs
(define (prdoc-addnewdoc prdcs newdoc contenido date)
  (prdoc-setdocs prdcs (cons (addnewversion newdoc ((prdoc-encrypter prdcs) contenido) date) (prdoc-docs prdcs)))
  )

;descripción: retira la sesión activa de paradigmadocs
;dominio: paradigmadocs
;recorrido: paradigmadocs
(define (prdoc-closesion prdcs)
  (prdoc-setsesion prdcs '())
  )

;descripción: extrae el nombre del usuario que se encuentra activo
;dominio: paradigmadocs
;recorrido: string(nombre usuario)
(define (prdoc-activeuser prdcs)
  (car (prdoc-sesion prdcs))
  )

;descripción: agrega diferentes accesos a un documento en particular
;dominio: paradigmadocs, int (id del documento), accesos
;recorrido: paradigmadocs
(define (prdoc-addmultiplyaccess2doc prdcs idDoc accesos)
  (define (addmultiplyaccess doc listacces)
    (if (eq? listacces null)
        doc
        (addmultiplyaccess (addaccess doc (car listacces)) (cdr listacces))
        )
    )
  (prdoc-setdocs prdcs (map (lambda (doc)
                              (if (and (docs-rightid? doc idDoc) (isowner? doc (prdoc-activeuser prdcs)))
                                  (addmultiplyaccess doc accesos)
                                  doc
                                  )
                              ) (prdoc-docs prdcs)))
  )

;descripción: agrega contenido a un documento en particular, concatenandolo al anterior
;dominio: paradigmadocs, int(id del documento a modificar), date, string(contenido) 
;recorrido: paradigmadocs
(define (prdoc-addcontent2somedoc prdcs idDoc date contenidoTexto)
  (prdoc-setdocs prdcs (map
                         (lambda(doc)
                           (if (docs-rightid? doc idDoc)
                               (if (canwrite? doc (prdoc-activeuser prdcs))
                                   (docs-restartmemory (addnewversionwithlast doc ((prdoc-encrypter prdcs) contenidoTexto) date))
                                   doc
                                   )
                               doc
                               )
                           )
                         (prdoc-docs prdcs))
                 )
  )

;descripción: restaura una versión en particular de un documento en particular siempre y cuando el usuario activo sea el owner
;dominio: paradigmadocs, int (id del documento), int (id de la versión)
;recorrido: paradigmadocs
(define (prdoc-restoreversion prdcs idDoc idVersion)
  (prdoc-setdocs prdcs (map (lambda(doc)
                               (if(docs-rightid? doc idDoc)
                                  (if(isowner? doc (prdoc-activeuser prdcs))
                                     (docs-restartmemory (addnewversion doc (version-content (docs-getsomeversion doc idVersion)) (version-date (docs-getsomeversion doc idVersion))))
                                     doc
                                     )
                                  doc
                                  )
                               )(prdoc-docs prdcs))
                 )
  )

;descripción: elimina todos los accesos de un documento. Realizable solo por el owner
;dominio: paradigmadocs
;recorrido: paradigmadocs
(define (prdoc-revokeallacceses prdcs)
  (prdoc-setdocs prdcs (map (lambda(doc)
                               (if(isowner? doc (prdoc-activeuser prdcs))
                                  (docs-kickall doc)
                                  doc
                                  )
                               )
                             (prdoc-docs prdcs)))
  )

;descripción: funciona que filta todos los documento de acuerdo a si contienen un trozo de información en particular
;dominio: paradigmadocs, string(texto buscado)
;recorrido:
(define (prdoc-filter4content prdcs searchText)
  (filter (lambda(doc)
                 (and (canread? doc (prdoc-activeuser prdcs)) (existcontentindoc? doc ((prdoc-encrypter prdcs) searchText)))
                    ) (prdoc-docs prdcs))
  )

;descripción: función que elimina una cantidad de letras en particular de la última versión de un documento en específico
;dominio: paradigmadocs, id, date, int (numero de caracteres a eliminar)
;recorrido: paradigmadocs
(define (prdoc-delete prdcs idDoc date numberOfCharecters)
  (prdoc-setdocs prdcs (map (lambda(doc)
                               (if(docs-rightid? doc idDoc)
                                  (if(canwrite? doc (prdoc-activeuser prdcs))
                                     (docs-restartmemory (doc-deletchars doc numberOfCharecters date (prdoc-encrypter prdcs) (prdoc-decrypter prdcs)))
                                     doc
                                     )
                                  doc
                                  )
                               )(prdoc-docs prdcs))
                 )
  )
;descripción: busca dentro de un documento en particular una cadena que reemplazará por otra cadena específica
;dominio: paradigmadocs, int(idDoc), date, string(palabra a buscar), string(palabra por la que se reemplazará)
;recorrido: paradigmadocs
(define (prdoc-searchreplace prdcs idDoc date searchText replaceText)
  (prdoc-setdocs prdcs (map (lambda(doc)
                               (if(docs-rightid? doc idDoc)
                                  (if(canwrite? doc (prdoc-activeuser prdcs))
                                     (docs-restartmemory (doc-searchreplace doc date ((prdoc-encrypter prdcs) searchText) ((prdoc-encrypter prdcs) replaceText)))
                                     doc
                                     )
                                  doc
                                  )
                               )(prdoc-docs prdcs))
                 )
  )
;descripción: aplica unos estilos específicos a un texto buscado
;dominio: paradigmadocs, int(idDoc), date, string(palabra a buscar), list(lista de estilos)
;recorrido: paradigmadocs
(define (prdoc-applystyles prdcs idDoc date searchText estilos)
  (prdoc-setdocs prdcs (map (lambda(doc)
                               (if(docs-rightid? doc idDoc)
                                  (if(canwrite? doc (prdoc-activeuser prdcs))
                                     (docs-restartmemory (doc-applystyles date doc searchText estilos (prdoc-encrypter prdcs) (prdoc-decrypter prdcs)))
                                     doc
                                     )
                                  doc
                                  )
                               )(prdoc-docs prdcs))
                 )
  )

;descripción: agrega un comentario a un documento en particular
;dominio: paradigmadocs, int(idDoc), date, string(palabra a comentar), string(comentario)
;recorrido: paradigmadocs
(define (prdoc-comment prdcs idDoc date selectedText commenText)
  (prdoc-setdocs prdcs (map (lambda(doc)
                               (if(docs-rightid? doc idDoc)
                                  (if(cancomment? doc (prdoc-activeuser prdcs))
                                     (doc-commentsometext doc date selectedText commenText (prdoc-encrypter prdcs) (prdoc-decrypter prdcs))
                                     doc
                                     )
                                  doc
                                  )
                               )(prdoc-docs prdcs))
                 )
  )
;descripción: traspasa una cantidad de versiones específicas de versiones a memory en un documento en particular
;dominio: paradigmadocs, int(id del documento), int(numero de veces a traspasar versiones)
;recorrido: paradigmadocs
(define (prdoc-ctrlZ prdcs idDoc numberOfUndo)
  (prdoc-setdocs prdcs (map (lambda(doc)
                               (if(docs-rightid? doc idDoc)
                                  (if(canwrite? doc (prdoc-activeuser prdcs))
                                     (doc-deshacer doc numberOfUndo)
                                     doc
                                     )
                                  doc
                                  )
                               )(prdoc-docs prdcs))
                 )
  )
;descripción: traspasa una cantidad de versiones específicas de memory a versiones en un documento en particular
;dominio: paradigmadocs, int(id del documento), int(numero de veces a traspasar versiones)
;recorrido: paradigmadocs
(define (prdoc-ctrlY prdcs idDoc numberOfRedo)
  (prdoc-setdocs prdcs (map (lambda(doc)
                               (if(docs-rightid? doc idDoc)
                                  (if(canwrite? doc (prdoc-activeuser prdcs))
                                     (doc-rehacer doc numberOfRedo)
                                     doc
                                     )
                                  doc
                                  )
                               )(prdoc-docs prdcs))
                 )
  )

;FUNCIONES QUE MUESTRAN CONTENIDO

;descrición: muestra los datos correspondientes al usuario ingresado, en el caos de no ingresar ningún usuario se mostrará información relacionada a todos los usuarios pertenecientes al paradigmadocs
;dominio: paradigmadocs, string (opcional, nombre de usuario)
;recorrido: string
;recursión: natural
(define (prdoc-showdatauser prdcs [user '()])
  (if(eq? user '())
     (if(eq?(prdoc-users prdcs) null)
        ""
        (string-append "usuario: " (user-name (car (prdoc-users prdcs))) "\n" "fecha de creación del usuario: " (date->string (user-date (car (prdoc-users prdcs)))) "\n" (prdoc-showdatauser (prdoc-setregisters prdcs (cdr (prdoc-users prdcs)))))
        )
     (if(eq? (user-name(car (prdoc-users prdcs))) user)
        (string-append "usuario: " user "\n" "fecha de creación del usuario: " (date->string (user-date (car (prdoc-users prdcs)))) "\n")
        (prdoc-showdatauser (prdoc-setregisters prdcs (cdr (prdoc-users prdcs))) user)
        )
     )
  )

;descripción: muestra todos los documentos del paradigmadocs, qué documento se muestre dependerá de si el usuario es el propietario o no, en el caso de no ingresar ningún usuario se mostrarán todos los documentos como si se fuera el propietario
;dominio: paradigmadocs, string(opcional, nombre de usuario)
;recorrido: string
;recursión: natural
(define (prdoc-showreadabledoc prdcs [user '()])
  (if(eq? user null)
     (if(eq? (prdoc-docs prdcs) null)
        ""
        (string-append (docs-readable (car (prdoc-docs prdcs)) (docs-owner (car (prdoc-docs prdcs))) (prdoc-decrypter prdcs)) (prdoc-showreadabledoc (prdoc-setdocs prdcs (cdr(prdoc-docs prdcs))) '()))
        )
     (if(eq? (prdoc-docs prdcs) null)
        ""
        (string-append (docs-readable (car (prdoc-docs prdcs)) user (prdoc-decrypter prdcs)) "\n" (prdoc-showreadabledoc (prdoc-setdocs prdcs (cdr(prdoc-docs prdcs))) user))
        )

     )
  )

(provide (all-defined-out))


