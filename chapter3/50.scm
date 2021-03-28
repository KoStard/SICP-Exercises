(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstream)
    (if (null? (car argstream))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstream))
            (apply stream-map (cons proc (map stream-cdr argstream)))
        )
    )
)