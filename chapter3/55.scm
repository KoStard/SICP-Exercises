(load "./chapter3/54.scm")


(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums s)
    (define res (cons-stream (stream-car s) (add-streams (stream-cdr s) res)))
    res
)

;;; (display (stream-ref (partial-sums integers) 4))