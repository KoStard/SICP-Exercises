(define (make-monitored f)
    (let ((count 0))
        (lambda (arg)
            (if (and (symbol? arg) (eq? arg 'how-many-calls?))
                count
                (begin
                    (set! count (+ count 1))
                    (f arg)
                )
            )
        )
    )
)


(define s (make-monitored sqrt))

(display (s 100)) (newline)
(display (s 400)) (newline)
(display (s 1600)) (newline)
(display (s 'how-many-calls?)) (newline)