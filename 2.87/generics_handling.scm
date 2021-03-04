;;; Loading the equal?
(load "./e2.54.scm")

(define implementation_mapping_table '(()))

(define (put generic_name implementation_key implementation)
    (set-cdr! implementation_mapping_table 
        (cons 
            (list (list generic_name implementation_key) implementation)
            (cdr implementation_mapping_table)
        )
    )
)

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (get generic_name implementation_key)
    (define (get_internal table)
        (if (null? table)
            false
            (if (equal? (caar table) (list generic_name implementation_key))
                (cadar table)
                (get_internal (cdr table))
            )
        )
    )
    (get_internal (cdr implementation_mapping_table))
)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

