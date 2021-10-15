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
;descripción: actualizar lista de documentos en paradigmadocs
;dominio: paradigmadocs, lista
;recorrido: paradigmadocs
(define (prdoc-setdocs prdocs newdocs)
  (list (prdoc-name prdocs) (prdoc-encrypter prdocs) (prdoc-decrypter prdocs) (prdoc-date prdocs) (prdoc-sesion prdocs) (prdoc-users prdocs) newdocs)
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
(define (prdcs-activeuser prdcs)
  (car (prdoc-sesion prdcs))
  )

(provide (all-defined-out))


