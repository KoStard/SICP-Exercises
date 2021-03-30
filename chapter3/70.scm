(load "./chapter3/65.scm")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
            (let ((s1-car-weight (weight (car s1car) (cadr s1car)))
                    (s2-car-weight (weight (car s2car) (cadr s2car))))
                (cond ((< s1-car-weight s2-car-weight)
                        (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                        ((> s1-car-weight s2-car-weight)
                        (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                        (else
                        (cons-stream s1car
                                    (merge-weighted (stream-cdr s1)
                                            s2 weight))))))))
)

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight) weight)))

;;; (display-first-n (weighted-pairs integers integers +) 50)
;;; (define non-divisibles (stream-filter (lambda (x) (and (> (remainder x 2) 0) (> (remainder x 3) 0) (> (remainder x 5) 0))) integers))
;;; (display-first-n (weighted-pairs non-divisibles non-divisibles (lambda (i j) (+ (* 2 i) (* 3 j) (* 5 i j)))) 50)