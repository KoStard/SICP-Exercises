(define (make_rat n d)
  (cons
    (if (and (or (negative? n) (negative? d))
             (or (not (negative? n)) (not (negative? d))))
             (- (abs n))
             (abs n)
             )
    (abs d))
)

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print_rat rat)
  (display (numer rat))
  (display '/)
  (display (denom rat))
  (newline)
)

(print_rat (make_rat 4 0))
