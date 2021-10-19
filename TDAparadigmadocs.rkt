#lang racket
(require "TDAfecha.rkt")
(require "EncryptFn_DencryptFn.rkt")
(require "TDAuser.rkt")
(require "TDAdocs.rkt")
(require "TDAaccess.rkt")

;Constructo de paradigmadocs
;dominio: str, date, EncryptFn, DencryptFn
;recorrido: lista compuesta por = nombre, fecha, EncryptFn, DencryptFn, sesión activa, lista de usuario y lista de documentos
(define paradigmadocs(lambda (name date encrypter decrypter)(
        list name date encrypter decrypter '() '() '()
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
  (list (prdoc-name prdocs) (prdoc-date prdocs) (prdoc-encrypter prdocs) (prdoc-decrypter prdocs) (prdoc-sesion prdocs) newregisters (prdoc-docs prdocs))
  )

;descripción: actualizar lista de sesiones en paradigmadocs
;dominio: paradigmadocs, lista
;recorrido: paradigmadocs
(define (prdoc-setsesion prdocs newsesion)
  (list (prdoc-name prdocs) (prdoc-date prdocs) (prdoc-encrypter prdocs) (prdoc-decrypter prdocs) newsesion (prdoc-users prdocs) (prdoc-docs prdocs))
  )
;descripción: actualizar lista de documentos en paradigmadocs
;dominio: paradigmadocs, lista
;recorrido: paradigmadocs
(define (prdoc-setdocs prdocs newdocs)
  (list (prdoc-name prdocs) (prdoc-date prdocs) (prdoc-encrypter prdocs) (prdoc-decrypter prdocs) (prdoc-sesion prdocs) (prdoc-users prdocs) newdocs)
  )

;descripción: verifica si existe algún usuario activo
;dominio: paradigmadocs
;recorrido: boolean
(define (prdoc-theresesion? prdcs)
  (not (eq? (prdoc-sesion prdcs) null))
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


