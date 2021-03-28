(load "./chapter3/61.scm")

;;; We have to handle s2 with non-zero constant term
;;; But the invert-unit-series supports series only with cons term of 1
;;; So we have to scale/descale it
(define (div-series s1 s2)
    (let ((s2-const-term (stream-car s2)))
        (if (= s2-const-term 0)
            (error "Denominator series with zero const term provide -- DIV-SERIES")
            (mul-series 
                (scale-stream s1 (/ 1 s2-const-term))
                (invert-unit-series (scale-stream s2 (/ 1 s2-const-term))))
        )
    )
)


;;; (display-first-n (div-series integers integers) 10)