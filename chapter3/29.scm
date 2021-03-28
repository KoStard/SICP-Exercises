;;; Define alternative or-gate with and-gates and inverters

(load "./chapter3/28.scm")

(define (or-gate a1 a2 output)
    (let (
        (b (make-wire))
        (c (make-wire))
        (d (make-wire))
    )
        (inverter a1 b)
        (inverter a2 c)
        (and-gate b c d)
        (inverter d output)
        'ok
    )
)

(let ((a (make-wire))
    (b (make-wire))
    (c (make-wire))
    )
    (set-signal! a 0)
    (set-signal! b 0)
    (or-gate a b c)
    (add-action! c (lambda () (display "Output ") (display (get-signal c)) (newline)))
    (set-signal! a 0)
    (set-signal! b 0)
)