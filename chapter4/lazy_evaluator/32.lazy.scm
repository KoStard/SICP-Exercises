(load "./chapter4/lazy_evaluator/base.scm")

; Whenever first argument should be delayed too, the lazy lists are the best options
; Also that's adding limitations that we have to put the delayed expressions in the cdr side
; e.g. we may want to change the tree structure to (left-branch value right-branch)
; But this would not be possible with the stream-based solution

(run-all
    '(
        (define (cons x y)
            (lambda (m) (m x y)))
        (define (car z)
            (z (lambda (p q) p)))
        (define (cdr z)
            (z (lambda (p q) q)))

        (define (endless-tree x)
            (cons (endless-tree x) (endless-tree x))
        )

        (display (endless-tree 1))
    )
)