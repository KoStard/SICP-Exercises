(load "./chapter4/lazy_evaluator/ch4-leval.scm")

(define (lazy-car z)
    ((cdr z) (lambda (p q) p)))
(define (lazy-cdr z)
    ((cdr z) (lambda (p q) q)))
(define (lazy-cons x y)
    (cons 'lazy-pair (lambda (m) (m x y))))

(define (lazy-pair? p)
    (and (pair? p) (tagged-list? p 'lazy-pair))
)



(define primitive-procedures
  (list (list 'car lazy-car)
        (list 'cdr lazy-cdr)
        (list 'cons lazy-cons)
        (list 'null? null?)
        (list 'list list)
        (list 'and (lambda (x y) (and x y)))
        (list 'or (lambda (x y) (or x y)))
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
        (list 'lazy-pair? lazy-pair?)
;;      more primitives
        ))

(define (native->lazy-list l)
    (if (null? l)
        '()
        (lazy-cons (process-content-under-quotation (car l)) (native->lazy-list (cdr l)))
        ; (list 'cons (car l) (native->lazy-list (cdr l)))
    )
)

(define (process-content-under-quotation content)
    (if (and (pair? content) (not (null? content)))
        (native->lazy-list content)
        content
    )
)

(define (text-of-quotation exp) 
    (let ((content (cadr exp)))
        (process-content-under-quotation content)
    )
)

(define the-global-environment (setup-environment))

(define (run-exp exp)
    (eval exp the-global-environment)
)

(define (run-all seq)
    (run-exp (car seq))
    (if (not (null? (cdr seq))) 
        (run-all (cdr seq))
    ))

(run-all '(
    ; With infinite lists, it's not possible to understand if this is a list of pairs
    ; So we have to print them in the same way, but somehow mark the end of the list of found -> '()
    ; (e1 , e2 , e3 , e4 , e5, ...)
    ; (e1, e2)
    ; (e1, e2, empty-list)
    (define (print-lazy-pair l elements-to-print)
        (define (internal-loop l elements-to-print)
            (cond 
                ((null? l) (display "<end>)"))
                ((= elements-to-print 0) (display "...)"))
                (else 
                    (enhanced-display (car l))
                    (display " , ")
                    (if (lazy-pair? (cdr l))
                        (internal-loop (cdr l) (- elements-to-print 1))
                        (display ")") ; it's not a list, but collection of pairs
                    )
                )
            )
        )
        (display "(")
        (internal-loop l elements-to-print)
    )

    (define (enhanced-display x)
        (if (lazy-pair? x)
            (print-lazy-pair x 5)
            (display x)
        )
    )

    (enhanced-display '(1 2 3 (4 (5)) 6 7 8 9 10))
))