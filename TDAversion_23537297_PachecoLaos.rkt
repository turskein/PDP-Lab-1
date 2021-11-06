#lang racket
(require "TDAcomentario_23537297_PachecoLaos.rkt")
;CONSTRUCTOR
;descripción: constructor TDA versión de la forma: (contendio , id)
;dominio: string, int, date
;recorrido: version
(define (version content id date)
  (list content id date '())
  )

;SELECTORES
;descripción: se obtiene el contenido de la versión
;dominio: versión
;recorrido: string
(define (version-content vrsn)
  (car vrsn)
  )

;descripción: se obtiene el id de la versión
;dominio: versión
;recorrido: integer
(define (version-id vrsn)
  (car (cdr vrsn))
  )

;descripción: retorna la fecha de una versión
;dominio: versión
;recorrido: date
(define (version-date vrsn)
  (car (cdr (cdr vrsn)))
  )

;descripción: retorna los comentarios de una versión en particular
;dominio: versión
;recorrido: comment's
(define (version-comment vrsn)
  (list-ref vrsn 3)
  )

;descripción: se obtiene el último comentario
;dominio: versión
;recorrido: comentario
(define (version-lastcomment vrsn)
  (car (version-comment vrsn))
  )

;MODIFICADORES
;descripción: setter de comentarios de las versiones
;dominio: versión, lista(lista de comentarios)
;recorrido: versión
(define (version-setcomment vrsn cmnts)
  (list (version-content vrsn) (version-id vrsn) (version-date vrsn) cmnts)
  )

;descripción: se agrega el contenido nuevo a la el contenido de una versión anterior, agregando en uno el ID
;dominio: versión (version anterior del documento), string (nuevo contenido), date
;recorrido: versión
(define (addcontent lastvrsn newcontent date)
  (version (string-append (version-content lastvrsn) newcontent) (+ 1 (version-id lastvrsn)) date)
  )
;descripción: agrega un comentario a la versión
;dominio: versión, comentario
;recorrido: versión
(define (version-addcomment vrsn cmnt)
  (version-setcomment vrsn (cons cmnt (version-comment vrsn)))
  )

;FUNCIONES ADICIONALES
;descripción: retorna el largo del contenido de una versión
;dominio: versión
;recorrido: int(largo del contenido de una versión)
(define (version-length vrsn)
  (string-length (version-content vrsn))
  )

(define (version-displaycomments vrsn decrypter)
  (if (null? (version-comment vrsn))
      ""
      (string-append (decrypter (com-selectedtext (version-lastcomment vrsn))) "->" (decrypter (com-commentext (version-lastcomment vrsn))) "/" (version-displaycomments (version-setcomment vrsn (cdr (version-comment vrsn))) decrypter))
      )
  )
(provide (all-defined-out))