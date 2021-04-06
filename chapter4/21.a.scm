;;; This is about passing the referrence to the lambda
;;; Without definition or assignment we can't just pass the referrence to the lambda

;;; ((lambda (f) (display f) (newline) (f f 1))
;;;     (lambda (l n) (display n) (newline) (l l (+ n 1)))
;;; )

;;; So we just create another lambda that will get the other lambda and call it with passing the referrence too

;;; Writing the fibonacci in this way

(display 
((lambda (n)
    (
        (lambda (f) (f f n))
        (lambda (f n) (cond ((= n 1) 1) ((= n 2) 1) (else (+ (f f (- n 1)) (f f (- n 2))))))
    )
) 10))