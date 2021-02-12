; regular accumulate is fold right, because it's calling op on item and the accumulation of everything right from that,
; so for every item it's combining with everything from it's right

; f(s1, f(s2, f(s3, .... f(sn, initial)...)))

; to achieve fold-left, we need to go iteratively, accumulate the initial item with the first one, the result with the second
; one, etc, until reaching the end

; f(...f(f(f(initial, s1), s2), s3), .... sn)


; with depth 1 it will be
; fold-right = f(s1, initial)
; fold-left = f(initial, s1)

; if we want to make them equal, then f(x, y) should be equal to f(y, x)

; with depth 2
; fold-right = f(s1, f(s2, initial))
; fold-left = f(f(initial, s1), s2)


; f(x, f(y, z)) = f(f(z, x), y)


; It has to match these two properties
; f(x, f(y, z)) = f(y, f(x, z))
; f(x, y) = f(y, x)


(load "./accumulate.scm")

(define fold_right accumulate)


(define (fold_left op initial sequence)
    (define (internal result current_sequence)
        (if (null? current_sequence)
            result
            (internal (op result (car current_sequence)) (cdr current_sequence))
        )
    )
    (internal initial sequence)
)


; (display (fold_left + 0 (list 1 2 3 4 5)))
; (newline)
;
;
; (display (fold_right / 1 (list 1 2 3))) (newline)
; (display (fold_left / 1 (list 1 2 3))) (newline)
; (display (fold_right list '() (list 1 2 3))) (newline)
; (display (fold_left list '() (list 1 2 3))) (newline)
