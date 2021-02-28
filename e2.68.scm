(load "./e2.67.scm")

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode_symbol (car message) tree)
            (encode (cdr message) tree))
    )
)

(define (contains? x set)
    (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (contains? x (cdr set))))
)

;;; Implement the encode_symbol
(define (encode_symbol symb tree)
    (if (leaf? tree)
        (if (eq? (symbol_leaf tree) symb) '()
            (error "reached a wrong leaf" tree)
        )
        (if (contains? symb (symbols (left_branch tree))) 
            (cons 0 (encode_symbol symb (left_branch tree)))
            (if (contains? symb (symbols (right_branch tree))) 
                (cons 1 (encode_symbol symb (right_branch tree)))
                (error "found unknown symbol" symb)
            )
        )
    )
)

;;; (display (decode (encode '(A B C D C B A) sample_tree) sample_tree)) (newline)