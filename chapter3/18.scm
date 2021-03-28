;;; Check if there are loops

(define (already-entered? x current-set)
    (and (not (null? current-set)) (or (eq? x (car current-set)) (already-entered? x (cdr current-set))))
)

(define (has-loops-internal? l current-set)
    (and (not (null? l)) (or (already-entered? l current-set) (has-loops-internal? (cdr l) (cons l current-set))))
)

(define (has-loops? l)
    (has-loops-internal? l '())
)

(define a '(a b c d))
;;; (set-cdr! (cdddr a) a)

(display (has-loops? a))
