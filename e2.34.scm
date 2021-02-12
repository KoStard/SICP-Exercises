; Horner's algorithm - optimal for computing polynomials
; It's great representation of polynomials!!!

(load "./e2.33.scm")

(define (horner_eval x coefficient_sequence)
    (accumulate (lambda (this_coeff higher_terms) (+ this_coeff (* x higher_terms)))
        0
        coefficient_sequence
    )
)


(display (horner_eval 2 (list 1 3 0 5 0 1)))
(newline)
