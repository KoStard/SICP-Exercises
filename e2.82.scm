;;; Generalize the apply-generic to handle multiple arguments
;;; Using the strategy described in the task, coerce everything to the type of the first argument, then second, then third, etc

(load "./e2.81.scm")
(load "./e2.59.scm")  ;;; Loading sets

(define (filter pred lst)
  (reverse (filter-help pred lst '())))

(define (filter-help pred lst res)
  (cond ((null? lst) res)
        ((pred (car lst)) 
           (filter-help pred (cdr lst)  (cons (car lst) res)))
        (else 
           (filter-help pred (cdr lst)  res))))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (let (
                    (distinct_types (list->set type-tags))
                )
                    (if (= (set_size distinct_types) 1)
                        ;;; We can't recursively call after coercion, as this will throw exception if nothing found
                        (error "Did not find any operation and not trying coercion as only one type is present")
                        (let (
                            (possible_types_with_coercion
                                (filter identity 
                                    (map (lambda (distinct_type)
                                        (if (= (length 
                                                (filter 
                                                    not   ; if not found, will be false
                                                    (map (lambda (argument_type_tag)
                                                        (if (eq? argument_type_tag distinct_type)
                                                            identity
                                                            (get-coercion argument_type_tag distinct_type)
                                                        )
                                                    ) type-tags)
                                                )
                                            ) 0) ; If is zero, then all types can be coerced to this type
                                            distinct_type
                                            false
                                        )
                                    ) distinct_types)
                                )
                            )
                        )
                            (let (
                                (possible_types_with_coersion_and_implementation
                                    (filter identity
                                        (map 
                                            (lambda (possible_type) 
                                                (if 
                                                    (get op 
                                                        ;;; Getting list of same size with the possible_type values
                                                        (map (lambda (type_tag) possible_type) type-tags)
                                                    )
                                                    possible_type
                                                    false
                                                )
                                            )
                                            possible_types_with_coercion
                                        )
                                    )
                                )
                            )
                                (if (null? possible_types_with_coersion_and_implementation)
                                    (error "Could not use coersion to find an implementation")
                                    (let ((target_type (car possible_types_with_coersion_and_implementation)))
                                        (let ((arguments (map (lambda (arg) (if (eq? (type-tag arg) target_type) arg ((get-coercion (type-tag arg) target_type) arg))) args)))
                                            (apply (get op (map type-tag arguments)) (map contents arguments))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

;;; (put 'print '(a a) (lambda (b c) (display b) (display " ") (display c) (newline)))
;;; (put-coercion 'b 'a (lambda (a) (attach-tag 'a (* (contents a) 2))))

;;; (apply-generic 'print (attach-tag 'a 20) (attach-tag 'b 4))
;;; (display (get-coercion 'a 'b))

