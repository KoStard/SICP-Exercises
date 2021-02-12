(define (make_mobile left right)
    (list left right)
)

(define (make_branch length structure)
    (list length structure)
)


; a

(define (left_branch mobile)
    (car mobile)
)

(define (right_branch mobile)
    (cadr mobile)
)

(define (branch_length branch)
    (car branch)
)

(define (branch_structure branch)
    (cadr branch)
)

; b

(define (total_weight mobile)
    ; no need to check for null, as we always have branches on mobiles and structures under branches
    (if (pair? mobile)
        (+ (total_weight (branch_structure (left_branch mobile))) (total_weight (branch_structure (right_branch mobile))))
        mobile
    )
)

; Test
; (display (total_weight (make_mobile (make_branch 1 5) (make_branch 2 (make_mobile (make_branch 4 4) (make_branch 1 1))))))


; c

(define (get_branch_torque branch)
    (* (branch_length branch) (total_weight (branch_structure branch)))
)

(define (balanced? mobile)
    (if (pair? mobile)
        (and (balanced? (branch_structure (left_branch mobile))) (balanced? (branch_structure (right_branch mobile)))
            (= (get_branch_torque (left_branch mobile)) (get_branch_torque (right_branch mobile)))
        )
        #t
    )
)

; Test -> Will return #f
; (display (balanced? (make_mobile (make_branch 1 1) (make_branch 0.5 (make_mobile (make_branch 1 1) (make_branch 0.5 1))))))
