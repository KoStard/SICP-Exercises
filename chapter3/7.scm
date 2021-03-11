(define (make-balance value)
    (define (withdraw amount)
        (if (>= value amount)
            (begin
                (set! value (- value amount))
                value)
            "Not enough balance"
        )
    )

    (define (deposit amount)
        (set! value (+ value amount))
        value
    )

    (lambda (message)
        (cond ((eq? message 'withdraw) withdraw)
            ((eq? message 'deposit) deposit)
            (else (error "Invalid message passed -- MAKE-ACCOUNT" message))
        )
    )
)

(define (make-account-with-balance balance password)
    (define (password-valid? given-password)
        (and (symbol? password) (eq? password given-password))
    )

    (lambda (message given-password)
        (if (password-valid? given-password)
            (if (eq? message 'get-balance)
                balance
                (balance message)
            )
            "Invalid password"
        )
    )
)

(define (make-account balance-value password)
    (make-account-with-balance (make-balance balance-value) password)
)

(define (make-joint account password new-password)
    (make-account-with-balance (account 'get-balance password) new-password)
)

(define peter-acc (make-account 25 'peter_password))
(define paul-acc (make-joint peter-acc 'peter_password 'paul_password))


(display ((peter-acc 'withdraw 'peter_password) 20)) (newline)
(display ((paul-acc 'withdraw 'paul_password) 4)) (newline)
