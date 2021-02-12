(load "./flatmap.scm")

(define (permutations sequence)
    (if (null? sequence)
        (list '())
        (flatmap (lambda (item)
            (map (lambda (permutation_without_item) (cons item permutation_without_item))
                (permutations
                    (remove (lambda (x) (= x item)) sequence)
                )
            )
        )
            sequence)
    )
)

; (display (permutations (list 1 2 3)))
