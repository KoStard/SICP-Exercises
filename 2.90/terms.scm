(define (install-terms-package)
    (define (tag p) (attach-tag 'term-list p))
    
    (define (negate terms)
        (if (empty-termlist? terms)
            terms  ; Not using the-empty-termlist to preserve the type
            (adjoin-term (negate_obj (first-term terms)) (negate (rest-terms terms)))
        )
    )
    (define (terms_zero? terms)
        (if (null? terms) 
            true 
            (and (=zero? (first-term terms))
                (terms_zero? (rest-terms terms))
            )
        )
    )
    (define (add-terms L1 L2)
        (cond ((empty-termlist? L1) L2)
                ((empty-termlist? L2) L1)
                (else
                (let ((t1 (first-term L1)) (t2 (first-term L2)))
                (cond ((> (order t1) (order t2))
                        (adjoin-term
                        t1 (add-terms (rest-terms L1) L2)))
                        ((< (order t1) (order t2))
                        (adjoin-term
                        t2 (add-terms L1 (rest-terms L2))))
                        (else
                        (adjoin-term
                        (make-term (order t1)
                                    (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                    (rest-terms L2)))))))))

    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            L1  ; Not using the-empty-termlist to preserve the type
            (add-terms (mul-term-by-all-terms (first-term L1) L2)
                        (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            L  ; Not using the-empty-termlist to preserve the type
            (let ((t2 (first-term L)))
                (adjoin-term
                (make-term (+ (order t1) (order t2))
                            (mul (coeff t1) (coeff t2)))
                (mul-term-by-all-terms t1 (rest-terms L))))))

    (put '=zero? '(term) terms_zero?)
    (put 'negate '(term-list) (lambda (term-list) (tag (negate term-list))))
    (put 'add '(term-list term-list) (lambda (L1 L2) (tag (add-terms L1 L2))))
    (put 'mul '(term-list term-list) (lambda (L1 L2) (tag (mul-terms L1 L2))))
    (put 'the-empty-sparse-termlist '() (lambda () (tag ((get 'the-empty-termlist 'sparse)))))
    (put 'the-empty-dense-termlist '() (lambda () (tag ((get 'the-empty-termlist 'dense)))))
    (put 'adjoin-term 'term-list (lambda (term term-list) (tag (adjoin-term term term-list))))

    'done)



(define (first-term terms) (apply-generic 'first-term terms))


;;; For the specific types
(define (adjoin-term term term-list) ((get 'adjoin-term (type-tag term-list)) term (contents term-list)))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (the-empty-sparse-termlist) ((get 'the-empty-sparse-termlist '())))
(define (the-empty-dense-termlist) ((get 'the-empty-dense-termlist '())))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
(define (negate_obj obj) (apply-generic 'negate obj))

;;; There is no coersion between these types, but we can add that too