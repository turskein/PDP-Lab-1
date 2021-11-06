#lang racket
;REPRESENTACIÖN
;string(texto seleccionado), string(comentario), date

;CONSTRUCTOR
;descripción: constructor de TDA comentario
;dominio: string(texto comentado), string(comentario), date
;recorrido: comentario
(define (comentario selectedText commenText date)
  (list selectedText commenText date)
  )

;SELECTORES
;descripción: se obtiene texto seleccionado del comentario
;dominio: comentario
;recorrido: string
(define (com-selectedtext cmmnt)
  (car cmmnt)
  )

;descripción: se obtiene texto comentario del comentario
;dominio: comentario
;recorrido: string
(define (com-commentext cmmnt)
  (car (cdr cmmnt))
  )

(provide (all-defined-out))