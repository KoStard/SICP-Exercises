;;; As we already have number? and symbol? type checks, we don't need type tags for these.
;;; As the system has to continue working as always, we should actually return some tags when called type_tag

(define (attach_tag type_tag contents)
    (cond 
        ((number? contents) contents)
        ((symbol? contents) contents)
        (else (cons type_tag contents))
    )
)

(define (type_tag datum)
    (cond 
        ((number? datum) 'scheme-number)
        ((symbol? datum) 'scheme-symbol)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE_TAG" datum))
    )
)

(define (contents datum)
    (cond 
        ((number? datum) datum)
        ((symbol? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))
    )
)

(display (type_tag (attach_tag 'number 3))) (newline)
(display (contents (attach_tag 'number 3))) (newline)
(display (type_tag (attach_tag 'symbol 'asd))) (newline)
(display (contents (attach_tag 'symbol 'asd))) (newline)
(display (type_tag (attach_tag 'custom_type '(1 2 3)))) (newline)
(display (contents (attach_tag 'custom_type '(1 2 3)))) (newline)
