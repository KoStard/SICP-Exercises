;;; Splitting into multiple files, as this one will probably require a lot of coding
;;; Types
;;; - polynomials
;;; - scheme numbers
;;; - rationals
;;; - complex
;;; We need the main basic logic for all of these packages and we want to add the =zero? for everything, including arithmetics
;;; Inside each of the files we are only defining the packages, but not installing, so after that we have to install them
;;; Initially including only the basics of the logic, 

(load "2.87/complex.scm")
(load "2.87/generics_handling.scm")
(load "2.87/rationals.scm")
(load "2.87/scheme-numbers.scm")
(load "2.91/polynomials.scm")
(load "2.90/term.scm")
(load "2.90/terms-dense.scm")
(load "2.90/terms-sparse.scm")
(load "2.91/terms.scm")

(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)
(install-polynomial-package)
(install-dense-terms-package)
(install-sparse-terms-package)
(install-terms-package)
(install-term-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ x y))
(define (=zero? x) (apply-generic '=zero? x))


(let (
    (tl1 (adjoin-term (make-term 4 (make-scheme-number 1)) (adjoin-term (make-term 2 (make-scheme-number -1)) (the-empty-sparse-termlist))))
    (tl2 (adjoin-term (make-term 2 (make-scheme-number 1)) (adjoin-term (make-term 0 (make-scheme-number -1)) (the-empty-sparse-termlist))))
)
    ;;; (display (=zero? (add (make_polynomial 'x tl1) (make_polynomial 'x tl2)))) (newline)
    (let (
        (p1 (make_polynomial 'x tl1))
        (p2 (make_polynomial 'x tl2))
    )
        (display (div p1 p2))
    )
)


;;; (display (adjoin-term (make-term (make-scheme-number 3) (make-scheme-number 3)) (adjoin-term (make-term (make-scheme-number 1) (make-scheme-number 3)) (the-empty-dense-termlist))))


;;; (display (adjoin-term (make-term (make-scheme-number 100) (make-scheme-number 3)) (the-empty-termlist)))