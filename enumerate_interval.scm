(define (enumerate_interval l r)
    (if (> l r)
        '()
        (cons l (enumerate_interval (+ l 1) r))
    )
)


; (display (enumerate_interval 1 5))
