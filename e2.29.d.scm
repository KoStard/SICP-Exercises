(load "./e2.29.scm")

; Thanks to abstraction barriers we have to change only these methods and the rest will work :)

(define (make_mobile left right)
    (cons left right)
)

(define (make_branch length structure)
    (cons length structure)
)

(define (left_branch mobile)
    (car mobile)
)

(define (right_branch mobile)
    (cdr mobile)
)

(define (branch_length branch)
    (car branch)
)

(define (branch_structure branch)
    (cdr branch)
)


; Test, will work as in the c subtask
; (display (balanced? (make_mobile (make_branch 1 1) (make_branch 0.5 (make_mobile (make_branch 1 1) (make_branch 0.5 1))))))
