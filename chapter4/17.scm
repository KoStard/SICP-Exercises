(load "./chapter4/16.scm")

;;; Alternative scan-out-defines, so that it doesn't create a separate frame
;;; For that we just need to go without let, just add defines in the beginning with unassigned values

(define (scan-out-defines body)
    (define (internal-loop current-body)
        (if (null? current-body)
            (cons (list ) (list ))  ; new body, the defines
            (let ((rest-processed (internal-loop (cdr current-body)))
                (first-exp (car current-body))
            )
                (if ((get-checker 'define) first-exp)
                    (cons 
                        (cons 
                            (make-assignment 
                                (definition-variable first-exp)
                                (definition-value first-exp)
                            )
                            (car rest-processed)
                        )
                        (cons
                            (definition-variable first-exp)
                            (cdr rest-processed)
                        )
                    )
                    (cons 
                        (cons first-exp (car rest-processed))
                        (cdr rest-processed)
                    )
                )
            )
        )
    )
    (define (add-defines defines body)
        (if (null? defines) body
            (add-defines (cdr defines) (cons (make-define (car defines) ''*unassigned*) body))
        )
    )
    (let ((processed (internal-loop body)))
        (if (null? (cdr processed))
            (car processed)
            (add-defines 
                (cdr processed)
                (car processed)
            )
        )
    )
)