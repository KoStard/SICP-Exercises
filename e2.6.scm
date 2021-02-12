(define zero (lambda (f) (lambda (x) x)))

(define (add_1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define (nd x) (display x) x)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; (((add_1 (add_1 (add_1 (add_1 (add_1 (add_1 zero)))))) nd) "x")
; (newline)

; ((two nd) "x")

(define (add n1 n2)
    (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)) ))
)


; (((add (add two one) two) nd) "x")


(define (mult n1 n2)
    (lambda (f) (lambda (x) ((n1 (n2 f)) x) ))
)
(((mult (mult two two) two) nd) "x")
