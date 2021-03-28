(load "./chapter3/65.scm")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs-from-stream-and-value stream value)
    (cons-stream (list (stream-car stream) value) (pairs-from-stream-and-value (stream-cdr stream) value))
)

(define (pairs-all s t)
    (cons-stream 
        (list (stream-car s) (stream-car t))
        (interleave (pairs-from-stream-and-value (stream-cdr s) (stream-car t)) (pairs-all s (stream-cdr t)))
    )
)

;;; (display-first-n (pairs-all integers integers) 10)