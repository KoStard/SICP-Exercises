; segment -> point -> number

; segments
(define (make_segment start_point end_point)
  (cons start_point end_point)
)

(define (start_segment segment)
  (car segment)
)

(define (end_segment segment)
  (cdr segment)
)

(define (length segment)
    (sqrt (+ (square (- (x_point (start_segment segment)) (x_point (end_segment segment)) ))
        (square (- (y_point (start_segment segment)) (y_point (end_segment segment)) ))
    ))
)

(define (print_segment segment)
    (display "Start")
    (newline)
    (print_point (start_segment segment))
    (display "End")
    (newline)
    (print_point (end_segment segment))
)

; points
(define (make_point x y)
  (cons x y)
)

(define (x_point point)
  (car point)
)

(define (y_point point)
  (cdr point)
)

; midpoint

(define (midpoint_segment segment)
  (make_point
    (/ (+ (x_point (start_segment segment))
        (x_point (end_segment segment))) 2)
    (/ (+ (y_point (start_segment segment))
        (y_point (end_segment segment))) 2)
  )
)

(define (print_point point)
  (display "(")
  (display (x_point point))
  (display ",")
  (display (y_point point))
  (display ")")
  (newline)
)

; (let ((start (make_point 5 5))
;       (end (make_point 0 0)))
;       (let (
;       (segment (make_segment start end))
;       )
;       (print_point (midpoint_segment segment))
;       ))
