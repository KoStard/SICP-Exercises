(load "./chapter3/70.scm")

(define (rand-update x)
    ;;; Very simple, probably very bad representation of random
    (remainder (+ (* x 7703) 6301) 5861)
)

(define rand-init 1)

(define (rand-private request-stream current)
    (let ((value 
        (let ((request (stream-car request-stream)))
            (cond ((eq? request 'generate) current)
                ((eq? request 'reset) rand-init)
                (else (error "Invalid request passed -- RAND" request))
            )
        )
    ))
        (cons-stream
            value
            (rand-private (stream-cdr request-stream) (rand-update value))
        )
    )
)

(define (rand request-stream)
    (rand-private request-stream rand-init)
)

(define (list-to-stream l)
    (if (null? l) '()
        (cons-stream (car l) (list-to-stream (cdr l)))
    )
)


(let ((requests (list-to-stream '(
    generate
    generate
    generate
    generate
    generate
    reset
    generate
    generate
    generate
    generate
    generate
))))
    (display-first-n (rand requests) 11)
)