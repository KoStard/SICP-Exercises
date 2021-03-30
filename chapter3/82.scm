(load "./chapter3/70.scm")

(define (random-stream x)
    (cons-stream (random x) (random-stream x))
)

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
    (let ((range (- high low)))
        (stream-map (lambda (x) (+ x low)) (random-stream range))
    )
)

(define (square-area x1 y1 x2 y2)
    (* (abs (- x2 x1)) (abs (- y2 y1)))
)

(define (estimate-integral P x1 x2 y1 y2)
    (let ((area (square-area x1 y1 x2 y2)))
        (stream-map 
            (lambda (pr) (* pr area))
            (monte-carlo (stream-map P (random-in-range x1 x2) (random-in-range y1 y2)) 0 0)
        )
    )
)

(define (in-the-circle-predicate cx cy r)
    (lambda (x y)
        (<= (+ (square (- x cx)) (square (- y cy))) (square r))
    )
)

(define pi 
    (stream-map (lambda (x) (/ x 25)) (estimate-integral (in-the-circle-predicate 5.0 5.0 5.0) 0.0 10.0 0.0 10.0)))

(display (stream-ref pi 500000))