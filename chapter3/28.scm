(define (make-wire)
    (list 0 (list ))
)

(define (get-signal wire) (car wire))
(define (get-all-actions wire) (cadr wire))
(define (call-all-actions actions) (if (not (null? actions)) (begin ((car actions)) (call-all-actions (cdr actions)))))
(define (set-signal! wire new-value) 
    (set-car! wire new-value) 
    (call-all-actions (get-all-actions wire))
)
(define (add-action! wire action)
    (set-car! (cdr wire) (cons action (get-all-actions wire)))
)
(define (after-delay d f)
    ;;; (display "Mock delay ") (display d) (newline)
    (f)
)
(define inverter-delay 1)
(define (inverter input output)
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
            (after-delay inverter-delay (lambda () (set-signal! output new-value)))
        )
    )
    (add-action! input invert-input)
    'ok
)
(define (logical-not a) (cond ((= a 0) 1) ((= a 1) 0) (else (error "Invalid signal" a))))
(define (logical-and a b) 
    (cond ((and (= a 1) (= b 1)) 1)
        ((and (or (= a 1) (= a 0)) (or (= b 1) (= b 0))) 0)
        (else (error "Invalid signal" (list a b))))
)
(define and-gate-delay 2)

(define (and-gate a1 a2 output)
    (define (and-action-procedure)
        (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
            (after-delay and-gate-delay (lambda () (set-signal! output new-value)))
        )
    )
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok
)
(define (logical-or a b) 
    (cond ((or (= a 1) (= b 1)) 1)
        ((and (= a 0) (= b 0)) 0)
        (else (error "Invalid signal" (list a b))))
)
(define or-gate-delay 2)

(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
            (after-delay or-gate-delay (lambda () (set-signal! output new-value)))
        )
    )
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok
)

;;; (let ((a (make-wire))
;;;     (b (make-wire))
;;;     (c (make-wire))
;;;     )
;;;     (set-signal! a 0)
;;;     (set-signal! b 0)
;;;     (or-gate a b c)
;;;     (add-action! c (lambda () (display "Output ") (display (get-signal c)) (newline)))
;;;     (set-signal! a 1)
;;;     (set-signal! b 1)
;;; )