(load "./chapter3/59.scm")

(define (mul-series s1 s2)
    (cons-stream 
        (* (stream-car s1) (stream-car s2)) 
        (add-streams 
            (scale-stream (stream-cdr s1) (stream-car s2))
            (mul-series s1 (stream-cdr s2)) ) )
)

(define (add-series s1 s2) (add-streams s1 s2))

;;; (display-first-n (mul-series sine-series sine-series) 10)
;;; (display-first-n (add-series (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)) 10)