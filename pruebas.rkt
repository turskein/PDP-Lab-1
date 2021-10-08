#lang racket
;Constructor TDA user
;dominio: string, string, date
;recorrido: user
(define (peo lis)(+ (car lis) (car (cdr lis))))
(define (peo2 num1 num2 num3)(+ num1 num2 num3))
(define peo3(lambda (fun)(if (eq? fun peo)
                             (lambda (x y)(fun x y))
                             (lambda (x y z)(fun x y z))
                             )
                             )
              )
(define peo4 (lambda (fun)(lambda (x . z) (fun z))))