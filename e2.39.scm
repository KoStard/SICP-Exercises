(load "./e2.38.scm")
(load "./e2.33.scm")


(define (reverse_fold_left sequence)
    (fold_left (lambda (x y) (cons y x)) '() sequence)
)

(define (reverse_fold_right sequence)
    (fold_right (lambda (x y) (append y (list x))) '() sequence)
)


(display (reverse_fold_left (list 1 2 3 4 5 6))) (newline)
(display (reverse_fold_right (list 1 2 3 4 5 6))) (newline)
