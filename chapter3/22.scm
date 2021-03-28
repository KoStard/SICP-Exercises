;;; Represent the queue as a procedure with local state

(define (make-queue)
    (let ((front-ptr '())
        (rear-ptr '()))

        (define (empty-queue?)
            (null? front-ptr)
        )

        (define (front-queue)
            (if (empty-queue?)
                (error "Called front on empty queue")
                (car front-ptr)
            )
        )

        (define (insert-queue! item)
            (let ((new-pair (cons item '())))
                (cond 
                    ((empty-queue?)
                        (set! front-ptr new-pair)
                        (set! rear-ptr new-pair)
                    )
                    (else 
                        (set-cdr! rear-ptr new-pair)
                        (set! rear-ptr new-pair)
                    )
                )
            )
        )

        (define (delete-queue!)
            (if (empty-queue?)
                (error "Called delete-queue! on empty queue")
                (set! front-ptr (cdr front-ptr))
            )
        )

        (define (dispatch message)
            (cond 
                ((eq? message 'empty-queue?) empty-queue?)
                ((eq? message 'front-queue) front-queue)
                ((eq? message 'insert-queue!) insert-queue!)
                ((eq? message 'delete-queue!) delete-queue!)
                (else (error "Unknown message passed to queue dispatch - " message))
            )
        )
        dispatch
    )
)



(let ((queue (make-queue)))
    (display ((queue 'empty-queue?))) (newline)
    ((queue 'insert-queue!) 1)
    ((queue 'insert-queue!) 2)
    ((queue 'insert-queue!) 3)
    ((queue 'insert-queue!) 4)
    ((queue 'insert-queue!) 5)
    (display ((queue 'front-queue))) (newline)
    ((queue 'delete-queue!))
    (display ((queue 'front-queue))) (newline)
    ((queue 'delete-queue!))
    (display ((queue 'front-queue))) (newline)
    ((queue 'delete-queue!))
    (display ((queue 'front-queue))) (newline)
    ((queue 'delete-queue!))
)