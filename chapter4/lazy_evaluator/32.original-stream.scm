(load "./chapter3/70.scm")

(define x (cons-stream 1 (cons x x)))

(display x)

(define (endless-tree x)
    (cons-stream (endless-tree x) (endless-tree x))
)

(endless-tree 1)