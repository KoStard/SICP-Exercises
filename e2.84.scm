;;; Implement apply-generic that will raise it's arguments to the same type
;;; Check which one is the "highest" and coerce everything to it

(load "./e2.83.scm")

(define (raisable type)
    (not (not (get 'raise (list type))))
)

(define (get_tower_depth x)
    (if (raisable (type-tag x))
        (+ 1 (get_tower_depth (raise x)))
        0
    )
)

(define (multi_raise x n)
    (if (= n 0)
        x
        (multi_raise (raise x) (- n 1))
    )
)

(define (apply-generic op . args)
    (let (
        (tower_depths (map get_tower_depth args))
    )
        (let ((min_tower_depth (apply min tower_depths)))
            (let ((processed_args (map (lambda (arg depth) (multi_raise arg (- depth min_tower_depth))) args tower_depths)))
                (let ((type-tags (map type-tag processed_args)))
                    (let ((proc (get op type-tags)))
                        (if proc
                            (apply proc (map contents processed_args))
                            (error "Could not find a procedure for given arguments - APPLY-GENERIC" processed_args)
                        )
                    )
                )
            )
        )
    )
)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-real-package)

(display (add (make-scheme-number 2) (make-rational 1 2)))