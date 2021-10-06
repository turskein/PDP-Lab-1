#lang racket
(require "TDAfecha.rkt")
(require "EncryptFn_DencryptFn.rkt")
;Constructo de paradigmadocs
;dominio: str, date, EncryptFn, DencryptFn
;recorrido: lista compuesta por = nombre, fecha, EncryptFn, DencryptFn, sesión activa, lista de usuario y lista de documentos
(define paradigmadocs(lambda name date EncryptFn DencryptFn(
        list name date EncryptFn DencryptFn "" '() '()
    )
))

;descripción: selector de nombre de paradigmadocs
;dominio: paradigmadocs
;recorrido: string
(define getName(lambda prdocs(
        car prdocs
   )
  )
 )
;descripción: selector de fecha de paradigmadocs
;dominio: paradigmadocs
;recorrido: lista (fecha)
(define getDate(lambda prdocs(
        car (cdr prdocs)
   )
  )
  )
;descripción: selector de usuarios de paradigmadocs
;dominio: paradigmadocs
;recorrido: lista de usuarios
(define getUsers(lambda prdocs(
        car (cdr (cdr  prdocs))
   )
  )
  )

;descripción: selector de documentos de paradigmadocs
;dominio: paradigmadocs
;recorrido: lista de documentos
(define getDocs(lambda prdocs(
        car (cdr (cdr (cdr (cdr (cdr (cdr prdocs))))))
   )
  )
 )

;descripción: agregar un usuario a lista de usuarios en paradigmadocs
;dominio: paradigmadocs y usuario a agregar
;recorrido: paradigmadocs


