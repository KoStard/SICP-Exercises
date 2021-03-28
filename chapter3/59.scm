(load "./chapter3/58.scm")

(define (integrate-series series)
    (define (integrate-series-iter series index)
        (cons-stream
            (/ (stream-car series) (+ 1 index))
            (integrate-series-iter (stream-cdr series) (+ 1 index))
        )
    )
    (integrate-series-iter series 0)
)

(define (negate stream) (cons-stream 
                            (- (stream-car stream)) 
                            (negate (stream-cdr stream))))

(define exp-series (cons-stream 1 (integrate-series exp-series)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (negate (integrate-series sine-series))))

;;; (display-first-n sine-series 10) (newline)
;;; (display-first-n cosine-series 10)