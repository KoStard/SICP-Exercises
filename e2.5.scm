; representing any pair nonnegative integers as 2**a * 3**b
; remainder


(define (extract_duos n)
    (i_extract_duos n 0)
)

(define (i_extract_duos n current)
    (if (= (remainder n 2) 0)
        (i_extract_duos (/ n 2) (+ current 1))
        current
    )
)

(define (extract_tres n)
    (i_extract_tres n 0)
)

(define (i_extract_tres n current)
    (if (= (remainder n 3) 0)
        (i_extract_tres (/ n 3) (+ current 1))
        current
    )
)

(define (pow x n current)
    (if (> n 0)
        (pow x (- n 1) (* x current))
        current
    )
)

; x -> 2s
; y -> 3s
(define (cons_int x y)
    (ash (pow 3 y 1) x)
)

(define (car_int z)
    (extract_duos z)
)

(define (cdr_int z)
    (extract_tres z)
)

; Trying out

(let (
    (z (cons_int 0 4))
    )
    (display (car_int z))
    (newline)
    (display (cdr_int z))
    (newline)
    )
