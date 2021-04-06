(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
    ;;;  lambda for even
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
    ;;;  lambda for odd
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(display (f 3))