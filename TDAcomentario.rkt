#lang racket
;descripción: constructor de TDA comentario
;dominio: string(texto comentado), string(comentario), date
;recorrido: comentario
(define (comentario selectedText commenText date)
  (list selectedText commenText date)
  )

(provide (all-defined-out))