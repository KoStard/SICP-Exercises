(define (install-sparse-terms-package)
    (define (tag p) (attach-tag 'sparse p))
    (define (adjoin-term term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))
    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))


    (put 'adjoin-term 'sparse (lambda (term term-list) (tag (adjoin-term term term-list))))
    (put 'the-empty-termlist 'sparse (lambda () (tag (the-empty-termlist))))
    (put 'first-term '(sparse) (lambda (term-list) (first-term term-list)))
    (put 'rest-terms '(sparse) (lambda (term-list) (tag (rest-terms term-list))))
    (put 'empty-termlist? '(sparse) empty-termlist?)
    'done)

;;; (define (the-empty-sparse-termlist)
;;;     ((get 'the-empty-termlist 'sparse))
;;; )