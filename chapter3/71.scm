(load "./chapter3/70.scm")

;;; Finding Ramanujan numbers
(define (cube x) (* x x x))

(define (find-same-neighbours stream)
    (let ((s0 (stream-car stream)) (s1 (stream-car (stream-cdr stream))))
        (if (= s0 s1)
            (cons-stream
                s1
                (find-same-neighbours (stream-cdr stream))
            )
            (find-same-neighbours (stream-cdr stream))
        )
    )
)

(define ramanujan-numbers (find-same-neighbours
        (stream-map
            (lambda (x) (+ (cube (car x)) (cube (cadr x))))
            (weighted-pairs integers integers (lambda (i j) (+ (cube i) (cube j))))
        )
    ))

;;; (display-first-n 
;;;     ramanujan-numbers
;;; 50)
