(load "./chapter3/66.scm")
(load "./chapter3/67.scm")


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (interleave-multiple s)
    (if (null? (cdr s))
        (car s)
        (interleave (car s) (interleave-multiple (cdr s)))
    )
)

(define (triples-all s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (let (
       (B (stream-map (lambda (tv) (list (stream-car s) tv (stream-car u))) (stream-cdr t)))
       (C (stream-map (lambda (sv) (list sv (stream-car t) (stream-car u))) (stream-cdr s)))
       (D (stream-map (lambda (uv) (list (stream-car s) (stream-car t) uv)) (stream-cdr u)))
       (E (stream-map (lambda (stp) (list (car stp) (cadr stp) (stream-car u))) (pairs-all (stream-cdr s) (stream-cdr t))))
       (F (stream-map (lambda (tup) (list (stream-car s) (car tup) (cadr tup))) (pairs-all (stream-cdr t) (stream-cdr u))))
       (G (stream-map (lambda (sup) (list (car sup) (stream-car t) (cadr sup))) (pairs-all (stream-cdr s) (stream-cdr u))))
   )
    (interleave-multiple
        (list 
            B C D E F G
            (triples-all (stream-cdr s) (stream-cdr t) (stream-cdr u))
        )
    )
   )
  )
)

;;; Triples where s <= t <= u
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (let (
    ;;;    (B (stream-map (lambda (tv) (list (stream-car s) tv (stream-car u))) (stream-cdr t)))
    ;;;    (C (stream-map (lambda (sv) (list sv (stream-car t) (stream-car u))) (stream-cdr s)))
       (D (stream-map (lambda (uv) (list (stream-car s) (stream-car t) uv)) (stream-cdr u)))
    ;;;    (E (stream-map (lambda (stp) (list (car stp) (cadr stp) (stream-car u))) (pairs (stream-cdr s) (stream-cdr t))))
       (F (stream-map (lambda (tup) (list (stream-car s) (car tup) (cadr tup))) (pairs (stream-cdr t) (stream-cdr u))))
    ;;;    (G (stream-map (lambda (sup) (list (car sup) (stream-car t) (cadr sup))) (pairs (stream-cdr s) (stream-cdr u))))
   )
    (interleave-multiple
        (list 
            D F
            (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
        )
    )
   )
  )
)

(define pythagorean-triples
    (stream-filter (lambda (x) (= (+ (square (car x)) (square (cadr x))) (square (caddr x)))) (triples integers integers integers))
)

;;; (display (stream-filter (lambda (x) (equal? x (list 4 3 2))) (triples-all integers integers integers)))
;;; (display (stream-filter (lambda (x) (equal? x (list 4 5 6))) (triples integers integers integers)))
(display-first-n pythagorean-triples 5)