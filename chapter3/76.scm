(load "./chapter3/70.scm")

(define (smooth stream)
    (scale-stream (stream-map + stream (stream-cdr stream)) 0.5)
)

(define zero-crossings
    (let ((smoothed-stream (smooth sense-data)))
        (stream-map sign-change-detector smoothed-stream (stream-cdr smoothed-stream))
    )
)