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


; access . accesses
;(define (doabig fact list)
;  ())
(define anotherone(lambda (acc . accs)
                    (if(eq? accs null)
                       (list acc)
                       accs
                       )))
;pruebas lambdas's en listas

(define list-opps (list peo peo2))
(define (enters-opps fun)(list (lambda (x y)(fun x y)) (lambda (x y z)(fun x y z))))

(define (rangeopps opp pos)
  (if (eq? (list-ref list-opps pos) opp)
      (list-ref (enters-opps opp) pos)
      (rangeopps opp (+ 1 pos))
  )
  )

((rangeopps peo2 0) 1 2 3)
