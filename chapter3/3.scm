(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin
                (set! balance (- balance amount))
                balance)
            "Not enough balance"
        )
    )

    (define (deposit amount)
        (set! balance (+ balance amount))
        balance
    )

    (define (password-valid? given-password)
        (and (symbol? password) (eq? password given-password))
    )

    (lambda (message given-password)
        (if (password-valid? given-password)
            (cond ((eq? message 'withdraw) withdraw)
                ((eq? message 'deposit) deposit)
                (else (error "Invalid message passed -- MAKE-ACCOUNT" message))
            )
            "Invalid password"
        )
    )
)

(define account (make-account 100 'some-password))
(display ((account 'withdraw 'some-password) 15)) (newline)
(display ((account 'withdraw 'some-password) 30)) (newline)
(display ((account 'withdraw 'some-password) 60)) (newline)