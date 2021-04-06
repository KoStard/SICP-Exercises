(define bindings (make-table))

;;; For every tag we should have checker and evaluator
(define (put tag proc-name implementation)
    (insert! (list tag proc-name) implementation bindings)
    'ok
)

(define (put-checker tag implementation)
    (put tag 'checker implementation)
)

(define (put-analyzer tag implementation)
    (put tag 'analyzer implementation)
)

(define (get tag proc-name)
    (lookup (list tag proc-name) bindings)
)

(define (get-checker tag)
    (if (symbol? tag) 
        (get tag 'checker)
        false
    )
)

(define (get-analyzer tag)
    (get tag 'analyzer)
)

(define (get-tag exp) (car exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
