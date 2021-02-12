(define (_fringe_internal items current)
    (cond ((null? items) current)
        ((not (pair? items)) (cons items current))
        (else
            (let ((right (cdr items))
                (left (car items))
                )
                (_fringe_internal left (_fringe_internal right current))
                )
            )
    )
)

(define (fringe items)
    (_fringe_internal items (list ))
)


(define x (list (list 1 (list 2 5 6) ) (list 3 4)))

(display (fringe x))
(newline)
(display (fringe (list x x)))
(newline)
