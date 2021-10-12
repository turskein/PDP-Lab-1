#lang racket
(require "TDAfecha.rkt")
(require "EncryptFn_DencryptFn.rkt")
(require "TDAuser.rkt")
(require "TDAdocs.rkt")
(require "TDAaccess.rkt")

;Constructo de paradigmadocs
;dominio: str, date, EncryptFn, DencryptFn
;recorrido: lista compuesta por = nombre, fecha, EncryptFn, DencryptFn, sesión activa, lista de usuario y lista de documentos
(define paradigmadocs(lambda (name date EncryptFn DencryptFn)(
        list name date EncryptFn DencryptFn '() '() '()
    )
))

;descripción: selector de nombre de paradigmadocs
;dominio: paradigmadocs
;recorrido: string
(define prdoc-name(lambda (prdocs)(
        car prdocs
   )
  )
 )

;descripción: selector de fecha de paradigmadocs
;dominio: paradigmadocs
;recorrido: lista (fecha)
(define prdoc-date(lambda (prdocs)(
        list-ref prdocs 1
   )
  )
  )

;descripción: selector de nombre de paradigmadocs
;dominio: paradigmadocs
;recorrido: string
(define prdoc-encrypter(lambda (prdocs)(
        list-ref prdocs 2
   )
  )
 )

;descripción: selector de nombre de paradigmadocs
;dominio: paradigmadocs
;recorrido: string
(define prdoc-decrypter(lambda (prdocs)(
        list-ref prdocs 3
   )
  )
 )

(define prdoc-sesion(lambda (prdocs)(
        list-ref prdocs 4
   )
  )
 )

;descripción: agregar un usuario a lista de usuarios en paradigmadocs
;dominio: paradigmadocs y usuario a agregar
;recorrido: paradigmadocs
(define prdoc-users(lambda (prdocs)(
    list-ref prdocs 5
)))
;descripción: selector de documentos de paradigmadocs
;dominio: paradigmadocs
;recorrido: lista de documentos
(define (prdoc-docs prdocs)(
        list-ref prdocs 6
   )
  )

;descripción: actualizar lista de usuarios registrados en paradigmadocs
;dominio: paradigmadocs, lista
;recorrido: paradigmadocs
(define (prdoc-setregisters prdocs newregisters)
  (list (prdoc-name prdocs) (prdoc-encrypter prdocs) (prdoc-decrypter prdocs) (prdoc-date prdocs) (prdoc-sesion prdocs) newregisters (prdoc-docs prdocs))
  )

;descripción: actualizar lista de sesiones en paradigmadocs
;dominio: paradigmadocs, lista
;recorrido: paradigmadocs
(define (prdoc-setsesion prdocs newsesion)
  (list (prdoc-name prdocs) (prdoc-encrypter prdocs) (prdoc-decrypter prdocs) (prdoc-date prdocs) newsesion (prdoc-users prdocs) (prdoc-docs prdocs))
  )

(define (prdoc-setdocs prdocs newdocs)
  (list (prdoc-name prdocs) (prdoc-encrypter prdocs) (prdoc-decrypter prdocs) (prdoc-date prdocs) (prdoc-sesion prdocs) (prdoc-users prdocs) newdocs)
  )

(define (prdoc-theresesion? prdcs)
  (not (eq? (prdoc-sesion prdcs) null))
  )

;
;---Fin Funciones paradigmadocs--------------------------------
;

;descripción: registrar un nuevo nuevo usuario a paradigmadocs
;dominio: paradigmadocs, date, string, string
;recorrido: paradigmadocs
;recursión: natural
 (define (register prdocs date name password)
   (define user1 (user name (EncryptFn password) date))
   (define (existUser? Users user1)
     (if(eq? Users null)
        #f
        (if(eqUser? (car Users) user1)
           #t
           (existUser? (cdr Users) user1)
        )
     ))
     (if(existUser? (prdoc-users prdocs) user1)
       prdocs
       (prdoc-setregisters prdocs (cons user1 (prdoc-users prdocs)))
   )
  )
;descripción: función que verifica si existe el usuario en la plataforma, en caso de existir verifica el password para poder aplicar una función y en el caso de no existir retorna el paradigmadocs sin modificaciones
;dominio: paradigmadocs, string, string, operación a aplicar
;recorrido: aplicación de operación o paradigmadocs
;recursión: natural
(define login(lambda (prdocs username password operation)
               (define user1 (user username (EncryptFn password) '(03 03 1980)))
               (define (isinUser? Users user1)
                 (if(eq? Users null)
                    #f
                    (if(eqUser? (car Users) user1)
                       (if (eqPass? (car Users) user1)
                           #t
                           #f
                           )
                       (isinUser? (cdr Users) user1)
                       )
                    ))
               
               (define prdoc_act (lazy (prdoc-setsesion prdocs (cons username (prdoc-sesion prdocs))))); ingreso de usuario a sesión
               (define list-opps (list create share add)); lista de todas las operaciones ejecutables a través de login
               (define enters-opps(list (lambda (date nombre contenido)(operation (force prdoc_act) date nombre contenido));entrada para create
                                                    (lambda (idDoc access . accesses)(operation (force prdoc_act) idDoc access accesses)); entrada para share
                                                    (lambda (idDoc date contenidoTexto)(operation (force prdoc_act) idDoc date contenidoTexto)); entrada para add
                                                    )
                 )
               (define (rangeopps pos)
                 (if(= pos (length list-opps))
                    prdocs
                    (if(eq? (list-ref list-opps pos) operation)
                       (list-ref enters-opps pos)
                       (rangeopps (+ 1 pos))
                       )
                    )
                 )
               (if(isinUser? (prdoc-users prdocs) user1)
                  (rangeopps 0)
                  prdocs
                  )
               )
  )

;descripción: generar un docs dentro del apartado 'docs' del paradigmadocs
;dominio: paradigmadocs, date, string, string
;recorrido: paradigmadocs
(define (create prdocs date nombre contenido)
  (define newdoc (lazy (docs nombre (car (prdoc-sesion prdocs)) (length (prdoc-docs prdocs)) date)))
  (if(prdoc-theresesion? prdocs)
     (prdoc-setsesion (prdoc-setdocs prdocs (cons (addnewversion (force newdoc) (EncryptFn contenido)) (prdoc-docs prdocs))) (cdr (prdoc-sesion prdocs)))
     prdocs
     )
  )

(define (share prdocs idDoc access . accesses)
  (define (addmultiplyaccess doc listacces)
    (if (eq? listacces null)
        doc
        (addmultiplyaccess (addaccess doc (car listacces)) (cdr listacces)))
        )
    
  (define addaccesses2doc
    (lambda (doc)
      (if (docs-rightid? doc idDoc)
          (addmultiplyaccess doc accesses)
          doc
          )
      )
    )


  (if(prdoc-theresesion? prdocs)
     (if(< idDoc (length (prdoc-docs prdocs)))
        (if(isowner? (list-ref (prdoc-docs prdocs) idDoc) (car (prdoc-sesion prdocs)))
           (prdoc-setsesion (prdoc-setdocs (prdoc-setdocs prdocs (map
                                  (lambda (doc)(if (docs-rightid? doc idDoc)
                                                   (addaccess doc access)
                                                   doc
                                                   )
                                    ) (prdoc-docs prdocs))) (map addaccesses2doc (prdoc-docs (prdoc-setdocs prdocs (map
                                  (lambda (doc)(if (docs-rightid? doc idDoc)
                                                   (addaccess doc access)
                                                   doc
                                                   )
                                    ) (prdoc-docs prdocs)))))) '())
           prdocs
           )
        prdocs
        )
     prdocs
     )
  )


(define add(lambda (x y z)(+ x y z)))
;-----Apliación de testeos
(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) EncryptFn DencryptFn))
(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3"))
(define gDocsn ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc0" "contenido doc0"))
(define gDocs2 ((login gDocsn "user1" "pass1" create) (date 30 08 2021) "doc1" "contenido doc1"))
(define gDocs3 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) "doc2" "contenido doc2"))
(define gDocs6 ((login gDocs2 "user1" "pass1" share) 0 (access "user1" #\r) (access "user2" #\w)))



