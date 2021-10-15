#lang racket
(require "TDAfecha.rkt")
(require "EncryptFn_DencryptFn.rkt")
(require "TDAuser.rkt")
(require "TDAdocs.rkt")
(require "TDAaccess.rkt")
(require "TDAparadigmadocs.rkt")

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
               (define enters-opps (list (lambda (date nombre contenido)(operation (force prdoc_act) date nombre contenido));entrada para create
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
     (prdoc-closesion (prdoc-setdocs prdocs (cons (addnewversion (force newdoc) (EncryptFn contenido) date) (prdoc-docs prdocs))))
     prdocs
     )
  )

(define (share prdocs idDoc access . accesses)
  (define (addmultiplyaccess doc listacces)
    (if (eq? listacces null)
        doc
        (addmultiplyaccess (addaccess doc (car listacces)) (cdr listacces))
        )
    )
    
  (define addaccesses2doc
    (lambda (doc)
      (if (docs-rightid? doc idDoc)
          (addmultiplyaccess doc newaccesses)
          doc
          )
      )
    )
  
  (define newaccesses (cons access (car accesses)))
  
  (if(and (prdoc-theresesion? prdocs) (< idDoc (length (prdoc-docs prdocs))) (isowner? (list-ref (prdoc-docs prdocs) idDoc) (car (prdoc-sesion prdocs))))
     (prdoc-closesion (prdoc-setdocs prdocs (map addaccesses2doc (prdoc-docs prdocs))))
     prdocs
     )
  )

;descripción: función que agrega contenido a un documento en particular
;dominio: paradigmadocs, int, date, string
;paradigmadocs:
(define (add prdocs idDoc date contenidoTexto)
  (if(prdoc-theresesion? prdocs)
     (prdoc-closesion (prdoc-setdocs prdocs (map
                                             (lambda(doc)
                                               (if (docs-rightid? doc idDoc)
                                                   (if (or (isowner? doc (prdcs-activeuser prdocs)) (canwrite? doc (prdcs-activeuser prdocs)))
                                                       (addnewversion doc (EncryptFn contenidoTexto) date)
                                                       doc
                                                       )
                                                   doc
                                                   )
                                               )
                                             (prdoc-docs prdocs))))
     (prdoc-closesion prdocs)
     )
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
(define gDocs8 ((login gDocs7 "user1" "pass1" add) 0 (date 30 11 2021) "mas contenido en doc0"))
(define gDocs9 ((login gDocs8 "user3" "pass3" add) 0 (date 30 11 2021) "mas contenido en doc3"))



