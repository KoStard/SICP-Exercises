(load "e2.54.scm")  ;;; Loading the equal? function

(define (element_of_set? x set)
    (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element_of_set? x (cdr set)))
    )
)

(define (adjoin_set x set)
    (if (element_of_set? x set)
        set
        (cons x set)
    )
)

(define (intersection_set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
        ((element_of_set? (car set1) set2)
            (cons (car set1) (intersection_set (cdr set1) set2))
        )
        (else (intersection_set (cdr set1) set2))
    )
)

(define (union_set set1 set2)
    (if (null? set2)
        set1
        (union_set (adjoin_set (car set2) set1) (cdr set2))
    )
)

(define (set_size set) (length set))

(define (list->set lst) 
    (if (null? lst) 
        lst
        (adjoin_set (car lst) (list->set (cdr lst)))
    )
)


;;; (display (intersection_set '(1 2 3 4) '(3 4 5 6)))
