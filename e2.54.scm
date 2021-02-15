(define (equal? a b)
    (cond ((and (not (pair? a)) (not (pair? b))) (if (and (number? a) (number? b)) (= a b) (eq? a b)))
        ((and (pair? a) (pair? b)) 
            (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
        )
        (else false)
    )
)

;;; (display (equal? '(a b c (d e f)) '(a b c (d e f)))) (newline)
;;; (display (equal? '(a b c d) '(a b c d e))) (newline)
;;; (display (equal? '(a b c (d e f)) '(a b c (d e f g)))) (newline)

;;; (display (equal? (list 1 2 3 4) (list 1 2 3 4))) (newline)