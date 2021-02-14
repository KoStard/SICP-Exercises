;;; (x + (3 * (x + (y + 2))))
;;; We need to process algebraic expressions in infix form, but expect that everything will be parenthesized


(load "./e2.56.scm")
;;; (display (deriv '(+ x (* 3 (+ x (+ y 2)))) 'x)) (newline)  ; Test - This has to be similar to the next output

;;; Without changing the deriv function everything has to work

(define (same_variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make_sum a1 a2) (list a1 '+ a2))
(define (make_product m1 m2) (list m1 '* m2))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))


;;; (display (deriv '(x + (3 * (x + (y + 2)))) 'x)) (newline)

