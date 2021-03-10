(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0) (/ trials-passed trials))
            ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else (iter (- trials-remaining 1) trials-passed))
        )
    )
    (iter trials 0.0)
)


(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))
    )
)

(define (square x) (* x x))


(define (in-the-circle-predicate cx cy r)
    (lambda (x y)
        (<= (+ (square (- x cx)) (square (- y cy))) (square r))
    )
)

(define (square-area x1 y1 x2 y2)
    (* (abs (- x2 x1)) (abs (- y2 y1)))
)


(define (estimate-integral p x1 y1 x2 y2 n)
    (* (monte-carlo n (lambda () (p (random-in-range x1 x2) (random-in-range y1 y2)))) (square-area x1 y1 x2 y2))
)


;;; (display (estimate-integral (lambda (x y) true) 0 0 10 10 5000))
;;; 3.14174592
(display (/ (estimate-integral (in-the-circle-predicate 5.0 5.0 5.0) 0.0 0.0 10.0 10.0 50000000) (square 5)))