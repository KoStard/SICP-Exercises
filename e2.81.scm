(load "./e2.80.scm")

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define coercion_table '(()))

(define (put-coercion generic_name implementation_key implementation)
    (set-cdr! coercion_table 
        (cons 
            (list (list generic_name implementation_key) implementation)
            (cdr coercion_table)
        )
    )
)

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (get-coercion generic_name implementation_key)
    (define (get_internal table)
        (if (null? table)
            false
            (if (equal? (caar table) (list generic_name implementation_key))
                (cadar table)
                (get_internal (cdr table))
            )
        )
    )
    (get_internal (cdr coercion_table))
)


;;; Limitted to 2 types
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                ;;; Continuing the checks only if the types are different
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                            (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                            (apply-generic op a1 (t2->t1 a2)))
                            (else
                            (error "No method for these types"
                                    (list op type-tags)))))
                )
              )
              (error "No method for these types"
                     (list op type-tags)))))))


;;; (define (scheme-number->scheme-number n) n)
;;; (define (complex->complex z) z)
;;; (put-coercion 'scheme-number 'scheme-number
;;;               scheme-number->scheme-number)
;;; (put-coercion 'complex 'complex complex->complex)

;;; If the operation is not found for these types, it will search and find the coercion from and to the same type, will coerce and call again
;;; This will result in infinite loop
;;; (display (apply-generic 'invalid_code (make-scheme-number 2) (make-scheme-number 4)))
;;; (display "This will never be printed")


;;; The only thing we may need, is to escalate the types of each of the arguments
;;; Otherwise, that's ok that it won't find conversion method for the same types, as if there was such implementation, 
;;; we would already find that with the first check