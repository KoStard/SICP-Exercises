; define unique_pairs such that 1 <= j < i <= n
; then use this function to simplify the prime_sum_pairs function

(load "./enumerate_interval.scm")
(load "./flatmap.scm")
(load "./prime.scm")

(define (unique_pairs n)
    (flatmap
        (lambda (i)
            (map
                (lambda (j) (list i j))
                (enumerate_interval 1 (- i 1)))
        )
        (enumerate_interval 1 n))
)


; (display (unique_pairs 5))


(define (prime_sum_pairs n)
    (filter
        (lambda (t) (prime? (caddr t)))
        (map
            (lambda (p) (list (car p) (cadr p) (+ (car p) (cadr p))))
            (unique_pairs n)))
)


; (display (prime_sum_pairs 8))
