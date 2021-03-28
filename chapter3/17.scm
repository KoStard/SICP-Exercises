;;; count-pair that will handle shared pairs too

(define (already-counted? x current-set)
    (and (not (null? current-set)) (or (eq? x (car current-set)) (already-counted? x (cdr current-set))))
)

(define (count-pairs-internal x current-set)
    (if (pair? x)
        (if (already-counted? x current-set)
            0
            (+ 
                1 
                (count-pairs-internal (car x) (cons x current-set))
                (count-pairs-internal (cdr x) (cons x current-set)))
        )
        0
    )
)

(define (count-pairs x)
    (count-pairs-internal x '())
)

(define a (cons (cons (cons 't 'c) 'b) 'a))
(set-car! (caar a) a)

(display (count-pairs a))
