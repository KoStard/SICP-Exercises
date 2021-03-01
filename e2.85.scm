;;; Implement drop function that will simplify the data object as far as possible
;;; For that we will also need project function, that will force the supertype to it's subtype
;;; And after that we will raise it again. If the result is the same (equ?), then this can be simplified.

(load "./e2.83.scm")

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
    (put 'raise '(scheme-number)
        (lambda (x) (make-rational x 1)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (and (= (numer x) 0) (not (= (denom x) 0)))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
    (put 'raise '(rational)
        (lambda (x) (make-real (/ (* 1.0 (numer x)) (denom x)))))
    (put 'project '(rational)
        (lambda (x) (make-scheme-number (numer x))))
  'done)

(define (install-real-package)
    (define (tag x) (attach-tag 'real x))
    (put 'make 'real (lambda (x) (tag x)))
    (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))
    (put 'equ '(real real)
        (lambda (x y) (= x y)))
    (put 'project '(real)
        (lambda (x) (make-rational (round (* x 1000)) 1000)))
)

(define (make-real x) ((get 'make 'real) x))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part x)
    (apply-generic 'real-part x))
  (define (imag-part x)
    (apply-generic 'imag-part x))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (apply-generic '=zero? x)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
    (put 'project '(complex)
        (lambda (x) (make-real (real-part x))))

  'done)

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-real-package)

(define (project x) 
    ((get 'project (list (type-tag x))) (contents x))
)

(define (projectable? x)
    (if (pair? x)
        (not (not (get 'project (list (type-tag x)))))
        false
    )
)

(define (drop x)
    (if (projectable? x)
        (if (equ? (raise (project x)) x)
            (drop (project x))
            x
        )
        x
    )
)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (drop (apply proc (map contents args)))
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
                                            (drop (apply (get op (map type-tag arguments)) (map contents arguments)))
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

;;; Had to change the raise operation, not to use the apply-generic, because now the apply generic is returning the dropped value, which is not
;;; what we want from the raise
(display (add (make-rational 3 2) (make-rational 5 2)))

;;; (let ((a (make-complex-from-real-imag 4 0)))
;;;     (display "-----") (newline)
;;;     (display (a)
;;; )