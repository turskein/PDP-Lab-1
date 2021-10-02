#lang racket

(define (date dia mes año)(
       if(and (integer? dia) (integer? mes) (integer? año))
       (if(and (and (> dia 0) (< dia 32)) (and (> mes 0) (< mes 32)) (and (> año 0) (< año 13)))
            (list dia mes año)
            null
            )
       null
    )
)

(define (setDay newDay Date)(
      (list newDay (car Date) (car Date))
  )
)