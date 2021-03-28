;;; a - implement semaphore with mutexes

(define (make-semaphore n)
    (let ((current-n n)
        (mut (make-mutex))
    )
        (define (acquire)
            (mut 'acquire)
            (if (> current-n 0)
                (begin 
                    (set! current-n (- current-n 1))
                    true
                )
                false
            )
            (mut 'release)
        )
        (define (release)
            (mut 'acquire)
            (set! current-n (+ current-n 1))
            (mut 'release)
        )
        (define (the-semaphore m)
            (cond ((eq? m 'acquire)
                    (if (not (acquire)) (the-semaphore m))
                )
                ((eq? m 'release)
                    (release)
                )
            )
        )
    )
)

;;; b

(define (make-semaphore n)
    (let ((current-n n)
        (cell (list false))
    )
        (define (acquire)
            (if (test-and-set! cell)
                (let (v (if (> current-n 0)
                    (begin 
                        (set! current-n (- current-n 1))
                        true
                    )
                    false
                ))
                    (set-car! cell false)
                    v)
                false
            )
        )
        (define (release)
            (if (test-and-set! cell)
                (begin
                    (set! current-n (+ current-n 1))
                    (set-car! cell false)
                    true
                )
                false
            )
        )
        (define (the-semaphore m)
            (cond ((eq? m 'acquire)
                    (if (not (acquire)) (the-semaphore m))
                )
                ((eq? m 'release)
                    (if (not (release)) (the-semaphore m))
                )
            )
        )
    )
)