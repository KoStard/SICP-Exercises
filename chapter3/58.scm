(load "./chapter3/56.scm")


(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


(define (display-first-n stream n)
    (if (> n 0)
        (begin
            (display (stream-car stream))
            (newline)
            (display-first-n (stream-cdr stream) (- n 1))
        )
    )
)

;;; (display-first-n (expand 3 8 10) 10)