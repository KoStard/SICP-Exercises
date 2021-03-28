(load "./chapter3/64.scm")

(define (ln-2-series-summands n)
    (cons-stream (/ 1.0 n)
        (negate (ln-2-series-summands (+ n 1)))
    )
)

(define ln-2-series
    (partial-sums (ln-2-series-summands 1))
)

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;;; (display-first-n (stream-car (make-tableau euler-transform ln-2-series)) 10)
;;; (newline)
;;; (display-first-n (stream-car (stream-cdr (make-tableau euler-transform ln-2-series))) 10)
;;; (newline)
;;; (display-first-n (stream-car (stream-cdr (stream-cdr (make-tableau euler-transform ln-2-series)))) 10)