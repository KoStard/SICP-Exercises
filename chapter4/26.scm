;;; (define (unless condition usual-value exceptional-value)
;;;     (if condition exceptional-value usual-value)
;;; )

;;; This doesn't work
;;; Because if is a special form and not a regular procedure that you can pass around
;;; So if the unless is implemented as a derived expression, then it will have the same limitation

(define (test f)
    (f true (display 1) (display 0))
)

(test if)