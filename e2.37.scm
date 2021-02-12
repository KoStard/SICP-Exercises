; WOW!

(load "./accumulate.scm")
(load "./e2.36.scm")

; sum of vi*wi
(define (dot-product v w)
    (accumulate + 0 (map * v w))
)

; Testing
; (display (dot-product (list 1 2 3) (list 4 5 6)))

; ti = sumj(mij*vj)
(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m)
)

; (display (matrix-*-vector (list (list 1 2 3 4) (list 5 6 7 8)) (list 9 10 11 12)))

(define (transpose m)
    (accumulate_n cons '() m)
)

; (display (transpose (list (list 7 8)
;     (list 9 10)
;     (list 11 12))))

(define (matrix-*-matrix m n)
    (let ((columns (transpose n)))
        (map (lambda (row)
            (matrix-*-vector columns row)
        ) m)
    )
)

(display (matrix-*-matrix
    (list (list 1 2 3)
        (list 4 5 6))
    (list (list 7 8)
        (list 9 10)
        (list 11 12))))
