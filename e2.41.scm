(load "./enumerate_interval.scm")
(load "./flatmap.scm")

; No additional filtration required

(define (find_all_triplets n s)
    (flatmap
        (lambda (i)
            (flatmap
                (lambda (j)
                    (map
                        (lambda (k)
                            (list i j k)
                        )
                        (enumerate_interval (max (+ j 1) (- s i j)) (min n (- s i j))))
                )
                (enumerate_interval (max (+ i 1) (- s n i)) (min n (- s i))))
        )
        (enumerate_interval 1 n))
)


; (display (find_all_triplets 36 100)) (newline)
