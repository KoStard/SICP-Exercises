;;; (+ (f 0) (f 1))
;;; 0 if left to right
;;; 1 if right to left


(define (get-f)
    (let ((p 0))
        (lambda (x)
            (let ((res p))
                (set! p x)
                res
            )
        )
    )
)

(let ((f (get-f)))
    (display (+ (f 0) (f 1))) (newline)
)
(let ((f (get-f)))
    (display (+ (f 1) (f 0))) (newline)
)