(load "./accumulate.scm")

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence)
)

; Test
; (display (map square (list 1 2 3 4 5)))


(define (append seq1 seq2)
    (accumulate cons seq2 seq1)
)

; (display (append (list 1 2 3) (list 4 5 6)))


(define (length sequence)
    (accumulate (lambda (x y) (+ y 1)) 0 sequence)
)

; (display (length (list 1 2 3 4 5)))
