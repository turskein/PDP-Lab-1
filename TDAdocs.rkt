#lang racket
;Constructor TDA user
;dominio: string, date, string
;recorrido: docs
(define (docs name date contents)
  (list name 0 date '() '())
  )