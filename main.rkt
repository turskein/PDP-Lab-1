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
(define login(lambda (prdcs username password operation)
               (define user1 (user username ((prdoc-encrypter prdcs) password) '(03 03 1980)))
               (define prdoc_act (lazy (prdoc-addactiveuser prdcs username))); generación de paradigmadocs con usuario a sesión
               (define list-opps (list create share add restoreVersion search delete searchAndReplace applyStyles)); lista de casi todas las operaciones ejecutables a través de login
               (define enters-opps
                 (lambda (applyprdoc)
                   (list (lambda (date nombre contenido)(operation applyprdoc date nombre contenido));entrada para create
                         (lambda (idDoc access . accesses)(operation applyprdoc idDoc access accesses)); entrada para share
                         (lambda (idDoc date contenidoTexto)(operation applyprdoc idDoc date contenidoTexto)); entrada para add
                         (lambda (idDoc idVersion)(operation applyprdoc idDoc idVersion));entrada restroreVersion
                         (lambda (searchText)(operation applyprdoc searchText));entrada searchText
                         (lambda (id date numberOfCharecters)(operation applyprdoc id date numberOfCharecters));entrada delete
                         (lambda (id date searchText replaceText)(operation applyprdoc id date searchText replaceText))
                         (lambda (id date searchText . styles)(operation applyprdoc id date searchText styles))
                         )
                   )
                 )
               (define (rangeopps pos applyprdoc) ;buscar coincidencia de operaciones recorriendolo a través de recursión de cola
                 (if(= pos (length list-opps))
                    prdcs
                    (if(eq? (list-ref list-opps pos) operation)
                       (list-ref (enters-opps applyprdoc) pos)
                       (rangeopps (+ 1 pos) applyprdoc)
                       )
                    )
                 )
               
               (if(rightuserpass? (prdoc-users prdcs) user1)
                  (if (or(eq? operation revokeAllAccesses) (eq? operation paradigmadocs->string))
                      (operation (force prdoc_act));aplicación función revoke o paradigmadocs->string
                      (rangeopps 0 (force prdoc_act))
                      )
                  (if (or(eq? operation revokeAllAccesses) (eq? operation paradigmadocs->string))
                      (operation prdcs);aplicación función revoke o paradigmadocs->string
                      (rangeopps 0 prdcs)
                      )
                  )
               )
  )

;descripción: generar un docs dentro del apartado 'docs' del paradigmadocs
;dominio: paradigmadocs, date, string, string
;recorrido: paradigmadocs
(define (create prdcs date nombre contenido)
  (define newdoc (lazy (docs nombre (prdoc-activeuser prdcs) (length (prdoc-docs prdcs)) date)))
  (if(prdoc-theresesion? prdcs)
     (prdoc-closesion (prdoc-addnewdoc prdcs (force newdoc) contenido date))
     prdcs
     )
  )
;descripción: función que genera accesos para diferentes usuario a un documento en particular
;dominio: paradigmadocs, int(id del documento), access, accesses(opcional, accesos adicionales)
;recorrido: paradigmadocs
(define (share prdcs idDoc access . accesses)
  (define newaccesses (cons access (car accesses)))
  
  (if(and (prdoc-theresesion? prdcs) (< idDoc (length (prdoc-docs prdcs))) )
     (prdoc-closesion (prdoc-addmultiplyaccess2doc prdcs idDoc newaccesses))
     prdcs
     )
  )

;descripción: función que agrega contenido a un documento en particular
;dominio: paradigmadocs, int, date, string
;paradigmadocs:
(define (add prdcs idDoc date contenidoTexto)
  (if(prdoc-theresesion? prdcs)
     (prdoc-closesion (prdoc-addcontent2somedoc prdcs idDoc date contenidoTexto))
     (prdoc-closesion prdcs)
     )
  )
;descripción: función que restaura una versión del documento indicado por su id
;dominio: paradigmadocs, int(id del documento a afectar), int(id de la versión a ser traída al tope)
;recorrido: paradigmadocs
(define restoreVersion
  (lambda (prdcs idDoc idVersion)
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
(define search
  (lambda (prdcs searchText)
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
(define (delete prdcs idDoc date numberOfCharecters)
  (if(prdoc-theresesion? prdcs)
     (prdoc-closesion (prdoc-delete prdcs idDoc date numberOfCharecters))
     prdcs
     )
  )
;descripción: busca dentro de un documento en particular una cadena que reemplazará por otra cadena específica
;dominio: paradigmadocs, int(idDoc), date, string(palabra a buscar), string(palabra por la que se reemplazará)
;recorrido: paradigmadocs
(define (searchAndReplace prdcs idDoc date searchText replaceText)
  (if(prdoc-theresesion? prdcs)
     (prdoc-closesion (prdoc-searchreplace prdcs idDoc date searchText replaceText))
     prdcs
     )
  )

;descripción: aplica unos estilos específicos a un texto buscado
;dominio: paradigmadocs, int(idDoc), date, string(palabra a buscar), list(lista de stilos que son caracteres)
;recorrido: paradigmadocs
(define (applyStyles prdcs idDoc date searchText . styles)
  (define estilos (map (lambda (stl)
         (string-append "#" "\u005C" (string stl))
         )(car styles)))
  (prdoc-closesion (prdoc-applystyles prdcs idDoc date searchText estilos))
  )

;-----Apliación de testeos
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
;((login gDocs11 "user1" "pass1" search) "contenido")
;(display (login gDocs14 "user1" "pass1" paradigmadocs->string))
;(display "\n\n")
(display (paradigmadocs->string gDocs14))




