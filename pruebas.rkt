#lang racket
;Constructor TDA user
;dominio: string, string, date
;recorrido: user
(define (peo num1 num2)(+ num1 num2))
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
(define (anotherone acc . accs)
                    accs
                       )
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
(define (ispar num)
  (if (= 0 (modulo num 2))
      (* num 2)
      #f
      )
  )








(define (suma num1 num2)(+ num1 num2))
(define (resta num1 num2)(- num1 num2))


(define (login function)
  (if(eq? function suma)
     (lambda (num1 num2)(function num1 num2))
     (if(eq? function resta)
        (lambda (num1 num2)(function num1 num2))
        0
        )
     )
  )

(define (sumauno num1)
  (if(= (modulo num1 2) 0)
     num1
     "peo"
     )
  )


(define salchipapa (+ 3 2))

(define salchicha
  (lambda (function)
  (if (procedure? function)
      (if(eq? function peo)
         (lambda (numero1 numero2)(function numero1 numero2))
         (lambda (numero1 numero2 numero3)(function numero1 numero2 numero3))
      )
      "kk"
      )
    )
  )



(define listaoperaciones (list
                          (lambda (num1 num2 num3)(+ num1 num2 num3))
                          (lambda (num1 num2)(- num1 num2))
                          (lambda (num1 num2)(< num1 num2))
                          )
  )

;testeos con filter

(define (existsubstring? lfword lfinword [init 0] [end (string-length lfword)])
  (if(> end (string-length lfinword))
     #f
     (if(string-ci=? (substring lfinword init end) lfword)
        #t
        (existsubstring? lfword lfinword (+ init 1) (+ end 1))
        )
     )
  )


(define test-strings (filter (lambda (x)(existsubstring? "peo" x 0 3)) '("peo" "cacapeo" "asdl")))


;ejemplos access
(define (cepillin x . j)
  (cons x j)
  )































