(define (rand-update x)
    ;;; Very simple, maybe very bad representation of random
    (remainder (+ (* x 7703) 6301) 5861)
)

(define rand
    (let ((x 1))
        (define (generate)
            (set! x (rand-update x))
            x
        )

        (define (reset y)
            (set! x y)
        )

        (lambda (message)
            (cond ((eq? message 'generate) generate)
                ((eq? message 'reset) reset)
                (else (error "Invalid message passed -- RAND" message))
            )
        )
    )
)


(display ((rand 'generate))) (newline)
(display ((rand 'generate))) (newline)
(display ((rand 'generate))) (newline)
(display ((rand 'generate))) (newline)
(display ((rand 'generate))) (newline)
((rand 'reset) 1) (newline)
(display ((rand 'generate))) (newline)
(display ((rand 'generate))) (newline)
(display ((rand 'generate))) (newline)
(display ((rand 'generate))) (newline)
(display ((rand 'generate))) (newline)