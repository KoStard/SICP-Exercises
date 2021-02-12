(load "./e2.11.scm")

(define (make_center_width center width)
    (make_interval (- center width) (+ center width))
)


(define (make_center_percent center percentage)
    (make_center_width center (* percentage center))
)

(define (center interval)
    (/ (+ (lower_bound interval) (upper_bound interval)) 2)
)

(define (width interval)
    (/ (- (upper_bound interval) (lower_bound interval)) 2)
)

(define (percentage interval)
    (/ (width interval) (center interval))
)
