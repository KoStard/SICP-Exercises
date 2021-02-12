(define (cc amount coin_values)
    (cond ((= amount 0) 1)
        ((or (< amount 0) (no_more? coin_values)) 0)
        (else
            (+ (cc amount (except_first_denomination coin_values))
                (cc (- amount (first_denomination coin_values)) coin_values)
            ))
    )
)

(define (no_more? l)
    (null? l)
)

(define (except_first_denomination l)
    (cdr l)
)

(define (first_denomination l)
    (car l)
)


; The order doesn't matter, as we are anyway going through all types of coints in the algorithm
; (define us_coins (list 50 25 10 5 1))
(define us_coins (list 1 5 10 25 50))
(define uk_coins (list 100 50 20 10 5 2 1 0.5))

(display (cc 100 us_coins))
(newline)
(display (cc 10 uk_coins))
(newline)
