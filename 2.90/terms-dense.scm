(define (install-dense-terms-package)
    (define (tag p) (attach-tag 'dense p))
    (define (adjoin-term term term-list)
        (if (=zero? (coeff term))
            term-list
            (let ((term_order (order term))
                (first_term_order (current-order term-list)))
                    (if (> term_order (+ first_term_order 1))
                        (adjoin-term term (list (+ (car term-list) 1) (cons 0 (cadr term-list))))
                        (list (+ (car term-list) 1) (cons (coeff term) (cadr term-list)))
                    ))))
    ;;; The length and the terms -> to understand the current order
    ;;; Starting from -1 to allow 0 order too
    (define (the-empty-termlist) (list -1 '()))
    (define (current-order term-list) (car term-list))
    (define (first-term term-list) (make-term (car term-list) (caadr term-list)))
    (define (rest-terms term-list) (list (- (car term-list) 1) (cdadr term-list)))
    (define (empty-termlist? term-list) (= (car term-list) -1))


    (put 'adjoin-term 'dense (lambda (term term-list) (tag (adjoin-term term term-list))))
    (put 'the-empty-termlist 'dense (lambda () (tag (the-empty-termlist))))
    (put 'first-term '(dense) (lambda (term-list) (first-term term-list)))
    (put 'rest-terms '(dense) (lambda (term-list) (tag (rest-terms term-list))))
    (put 'empty-termlist? '(dense) empty-termlist?)
    'done)

;;; (define (the-empty-dense-termlist)
;;;     ((get 'the-empty-termlist 'dense))
;;; )