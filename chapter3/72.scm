(load "./chapter3/70.scm")

(define (square x) (* x x))

(define (find-same-neighbours stream neighbours-count func)
    (define (find-same-neighbours-iter stream last-value current-pairs current-count)
        (let ((matches-last-value (= last-value (func (stream-car stream)))))
            (let ((new-count (if matches-last-value (+ current-count 1) 1))
                (new-pairs (if matches-last-value (cons (stream-car stream) current-pairs) (list (stream-car stream))))
            )
                (if (and (>= current-count neighbours-count) (not matches-last-value))
                    (cons-stream 
                        current-pairs
                        (find-same-neighbours-iter (stream-cdr stream) (func (stream-car stream)) new-pairs new-count)
                    )
                    (find-same-neighbours-iter (stream-cdr stream) (func (stream-car stream)) new-pairs new-count)
                )
            )
        )
    )
    (find-same-neighbours-iter (stream-cdr stream) (func (stream-car stream)) (list (stream-car stream)) 1)
)

(display-first-n 
    (find-same-neighbours
        (weighted-pairs integers integers (lambda (i j) (+ (square i) (square j))))
        3
        (lambda (x) (+ (square (car x)) (square (cadr x))))
    )
50)
