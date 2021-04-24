; The current implementation of amb is using DFS, but that is not good enough for dealing with infinite
; sets. So we just have to write a procedure that will trick the amb and will use BFS.

(define (all-pythagorean-triples)
    (define (filter proc l)
        (if (null? l) l 
            (if (proc (car l))
                (cons (car l) (filter proc (cdr l)))
                (filter proc (cdr l))
            )
        )
    )
    (define (get-next-transformations element)
        (let ((i (car element)) (j (cadr element)) (k (caddr element)))
            (filter valid? 
                (list 
                    (list (+ i 1) j k)
                    (list i (+ j 1) j)
                    (list i j (+ k 1))
                )
            )
        )
    )
    (define (valid? element)
        (let ((i (car element)) (j (cadr element)) (k (caddr element)))
            (and (< i j) (< j k))
        )
    )
    (define (internal-loop queue)
        (let ((first ((queue 'front-queue))))
            ((queue 'delete-queue!))
            (for-each 
                (queue 'insert-queue!)
                (get-next-transformations first)
            )
            ; Time splits here
            ; Sometimes it's just returning the value of first
            ; Sometimes it's trying again, getting the next, etc...
            (amb first (internal-loop queue))
        )
    )
    (define (is-pythagorean-triple i j k)
        (= (+ (* i i) (* j j)) (* k k))
    )
    (let ((q (make-queue)))
        ((q 'insert-queue!) (list 1 2 3))
        (let ((el (internal-loop q)))
            (require (is-pythagorean-triple (car el) (cadr el) (caddr el)))
            el
        )
    )
)