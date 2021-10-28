#lang racket
(require "TDAfecha.rkt")
(require "EncryptFn_DencryptFn.rkt")
(require "TDAuser.rkt")
(require "TDAversion.rkt")
(require "TDAdocs.rkt")
(require "TDAaccess.rkt")
(require "TDAparadigmadocs.rkt")

;descripción: registrar un nuevo nuevo usuario a paradigmadocs
;dominio: paradigmadocs, date, string, string
;recorrido: paradigmadocs
;recursión: natural
(define (register prdcs date name password)
  (define user1 (user name ((prdoc-encrypter prdcs) password) date))
  (prdoc-setregisters prdcs (prdoc-adduser (prdoc-users prdcs) user1))
  )

;descripción: función que verifica si existe el usuario en la plataforma, en caso de existir verifica el password para poder aplicar una función y en el caso de no existir retorna el paradigmadocs sin modificaciones
;dominio: paradigmadocs, string, string, operación a aplicar
;recorrido: aplicación de operación o paradigmadocs
;recursión: cola
(define login
  (lambda (prdcs username password operation)
    (let ([user1 (user username ((prdoc-encrypter prdcs) password) (date 10 10 2012))])
      (if (rightuserpass? (prdoc-users prdcs) user1)
          (operation (prdoc-addactiveuser prdcs username))
          operation
          )
      )
    )
  )

;descripción: generar un docs dentro del apartado 'docs' del paradigmadocs
;dominio: paradigmadocs, date, string, string
;recorrido: paradigmadocs
(define (create prdcs)
  (lambda (date nombre contenido)
    (let ([newdoc (lazy (docs nombre (prdoc-activeuser prdcs) (length (prdoc-docs prdcs)) date))])
      (if(prdoc-theresesion? prdcs)
         (prdoc-closesion (prdoc-addnewdoc prdcs (force newdoc) contenido date))
         prdcs
         )
      )
    )
  )
;descripción: función que genera accesos para diferentes usuario a un documento en particular
;dominio: paradigmadocs, int(id del documento), access, accesses(opcional, accesos adicionales)
;recorrido: paradigmadocs
(define (share prdcs)
  (lambda (idDoc access . accesses)
    (define newaccesses (cons access accesses))
    (if(and (prdoc-theresesion? prdcs) (< idDoc (length (prdoc-docs prdcs))) )
       (prdoc-closesion (prdoc-addmultiplyaccess2doc prdcs idDoc newaccesses))
       prdcs
       )
    )
  )
;descripción: función que agrega contenido a un documento en particular
;dominio: paradigmadocs, int, date, string
;paradigmadocs:
(define (add prdcs)
  (lambda (idDoc date contenidoTexto)
    (if(prdoc-theresesion? prdcs)
       (prdoc-closesion (prdoc-addcontent2somedoc prdcs idDoc date contenidoTexto))
       (prdoc-closesion prdcs)
       )
    )
  )
;descripción: función que restaura una versión del documento indicado por su id
;dominio: paradigmadocs, int(id del documento a afectar), int(id de la versión a ser traída al tope)
;recorrido: paradigmadocs
(define (restoreVersion prdcs)
  (lambda (idDoc idVersion)
    (if (prdoc-theresesion? prdcs)
        (prdoc-closesion (prdoc-restoreversion prdcs idDoc idVersion))
        prdcs
        )
    )
  )
;descripción: función que elimina todos los accesos existentes en los documentos pertenientes al usuario activo
;dominio: paradigmadocs
;recorrido: paradigmadocs
(define revokeAllAccesses
  (lambda (prdcs)
    (if (prdoc-theresesion? prdcs)
        (prdoc-closesion (prdoc-revokeallacceses prdcs))
        prdcs
        )
    )
  )
;descripción: retorna una lista con todos los documentos que contienen una cadena de texto en particular, siempre y cuando se puede leer el archivo
;dominio: paradigmadocs, string(cadena de texto a buscar)
;recorrido: list(lista con los documentos)
(define (search prdcs)
  (lambda (searchText)
    (if(prdoc-theresesion? prdcs)
       (prdoc-filter4content prdcs searchText)
       null
       )
    )
  )

;descripción: muestra todo lo relacionado al paradigmadoc operado, mostrando los diferentes usuarios. documentos y datos
;dominio: paradigmadocs
;recorrido: string
(define paradigmadocs->string
  (lambda (prdcs)
    (if(prdoc-theresesion? prdcs)
       (string-append "Plataforma: " (prdoc-name prdcs) "\n" "-----Usuario-----" "\n" (prdoc-showdatauser prdcs (prdoc-activeuser prdcs))"-------Doc(s)-------" "\n" (prdoc-showreadabledoc prdcs (prdoc-activeuser prdcs)))
       (string-append "Plataforma: " (prdoc-name prdcs) "\n" "----Usuario(s)----" "\n" (prdoc-showdatauser prdcs) "-------Doc(s)-------" "\n" (prdoc-showreadabledoc prdcs))
       )
    )
  )
;descripción: función que elimina una cantidad específica de caracteres de un documento en particular
;dominio: paradigmadocs, int(id del documento), date, int(numero de caracteres a eliminar)
;recorrido: paradigmadocs
(define (delete prdcs)
  (lambda (idDoc date numberOfCharecters)
    (if(prdoc-theresesion? prdcs)
     (prdoc-closesion (prdoc-delete prdcs idDoc date numberOfCharecters))
     prdcs
     )
    )
  )
;descripción: busca dentro de un documento en particular una cadena que reemplazará por otra cadena específica
;dominio: paradigmadocs, int(idDoc), date, string(palabra a buscar), string(palabra por la que se reemplazará)
;recorrido: paradigmadocs
(define (searchAndReplace prdcs)
  (lambda (idDoc date searchText replaceText)
    (if(prdoc-theresesion? prdcs)
       (prdoc-closesion (prdoc-searchreplace prdcs idDoc date searchText replaceText))
       prdcs
       )
    )
  )

;descripción: aplica unos estilos específicos a un texto buscado
;dominio: paradigmadocs, int(idDoc), date, string(palabra a buscar), list(lista de stilos que son caracteres)
;recorrido: paradigmadocs
(define (applyStyles prdcs)
  (lambda (idDoc date searchText . styles)
    (define estilos (map (lambda (stl)
                         (string-append "#" "\u005C" (string stl))
                         )styles))
    (if(prdoc-theresesion? prdcs)
       (prdoc-closesion (prdoc-applystyles prdcs idDoc date searchText estilos))
       prdcs
       )
    )
  )
;descripción: agrega un comentario a un documento en particular
;dominio: paradigmadocs, int(idDoc), date, string(palabra a comentar), string(comentario)
;recorrido: paradigmadocs
(define (comment prdcs)
  (lambda(idDoc date selectedText commenText)
    (if(prdoc-theresesion? prdcs)
       (prdoc-closesion (prdoc-comment prdcs idDoc date selectedText commenText))
       prdcs
       )
    )
  )
;descripción: deshace una cantidad determinada de modificaciones sobre el contenido de un documento
;dominio: paradigmadocs, int(id del documento), int(numero de veces a traspasar versiones)
;recorrido: paradigmadocs
(define (ctrlZ prdcs)
  (lambda (idDoc numberOfUndo)
    (if(prdoc-theresesion? prdcs)
       (prdoc-closesion (prdoc-ctrlZ prdcs idDoc numberOfUndo))
       prdcs
       )
    )
  )
;descripción: rehace una cantidad determinada de modificaciones sobre el contenido de un documento
;dominio: paradigmadocs, int(id del documento), int(numero de veces a traspasar versiones)
;recorrido: paradigmadocs
(define (ctrlY prdcs)
  (lambda (idDoc numberOfRedo)
    (if(prdoc-theresesion? prdcs)
       (prdoc-closesion (prdoc-ctrlY prdcs idDoc numberOfRedo))
       prdcs
       )
    )
  )


;-----Aplicación de testeos
(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) EncryptFn DencryptFn))
(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3"))
(define gDocsn ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc0" "contenido doc0"))
(define gDocs2 ((login gDocsn "user1" "pass1" create) (date 30 08 2021) "doc1" "contenido doc1"))
(define gDocs3 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) "doc2" "contenido doc2"))
(define gDocs6 ((login gDocs3 "user1" "pass1" share) 0 (access "user1" #\r) (access "user2" #\w)(access "user3" #\c)))
(define gDocs7 ((login gDocs6 "user3" "pass3" share) 0 (access "user1" #\c)))
(define gDocs8 ((login gDocs7 "user1" "pass1" add) 0 (date 30 11 2021) " mas contenido en doc0"))
(define gDocs9 ((login gDocs8 "user3" "pass3" add) 0 (date 30 11 2021) " mas contenido en doc3"))
(define gDocs10 ((login gDocs9 "user1" "pass1" restoreVersion) 0 0))
(define gDocs11 (login gDocs10 "user2" "pass2" revokeAllAccesses))
(define gDocs12 ((login gDocs11 "user1" "pass1" delete) 1 (date 30 11 2021) 5))
(define gDocs13 ((login gDocs12 "user3" "pass3" searchAndReplace) 0 (date 30 11 2021) "contenido" "content"))
(define gDocs14 ((login gDocs13 "user2" "pass2" applyStyles) 0 (date 30 11 2021) "content" #\b #\i))
(define gDocs15 ((login gDocs14 "user1" "pass1" comment) 0 (date 30 12 2021) "contenido" "Este es un comentario"))
(define gDocs16 ((login gDocs15 "user1" "pass1" add) 0 (date 30 11 2021) " este es "))
(define gDocs17 ((login gDocs16 "user1" "pass1" add) 0 (date 30 11 2021) "un nuevo "))
(define gDocs18 ((login gDocs17 "user1" "pass1" add) 0 (date 30 11 2021) "text en doc0"))
(define gDocs19 ((login gDocs18 "user1" "pass1" ctrlZ) 0 5))
(define gDocs20 ((login gDocs19 "user1" "pass1" ctrlY) 0 2))
(define gDocs21 ((login gDocs20 "user1" "pass1" add) 0 (date 30 11 2021) " este es "))

;((login gDocs11 "user1" "pass1" search) "contenido")
;(display (login gDocs14 "user1" "pass1" paradigmadocs->string))
;(display "\n\n")
;(display (paradigmadocs->string gDocs14))




