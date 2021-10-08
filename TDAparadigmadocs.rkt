#lang racket
(require "TDAfecha.rkt")
(require "EncryptFn_DencryptFn.rkt")
(require "TDAuser.rkt")
;Constructo de paradigmadocs
;dominio: str, date, EncryptFn, DencryptFn
;recorrido: lista compuesta por = nombre, fecha, EncryptFn, DencryptFn, sesión activa, lista de usuario y lista de documentos
(define paradigmadocs(lambda (name date EncryptFn DencryptFn)(
        list name date EncryptFn DencryptFn "" '() '()
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
       (list (prdoc-name prdocs) (prdoc-encrypter prdocs) (prdoc-decrypter prdocs) (prdoc-date prdocs) (prdoc-sesion prdocs) (cons user1 (prdoc-users prdocs)) (prdoc-docs prdocs))
   )
  )

(define login(lambda (prdocs username password operation)
    (define user1 (username (EncryptFn password) (03 03 1980)))
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
     (if(isinUser? (prdoc-users) user1)
        (if(eq? operation create)
           (lambda (date nombre contenido)(operation prdocs date nombre contenido))
           (if(eq? operation share)
              (lambda (idDoc access . accesses)(operation prdocs idDoc access accesses))
              (if(eq? operation add)
                 (lambda (idDoc date contenidoTexto)(operation prdocs idDoc date contenidoTexto))
                 (lambda (idDoc idVersion)(operation idDoc idVersion))
                 )
              )
           )
        prdocs
        )
               )
  )


(define create(lambda (x y z)(+ x y z)))
(define share(lambda (x y z)(+ x y z)))
(define add(lambda (x y z)(+ x y z)))
