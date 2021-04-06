(load "./chapter4/13.scm")

(define (setup-environment)
  (let ((initial-env
         (extend-environment primitive-procedures
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (tag-primitive proc) (list 'primitive proc))

(define primitive-procedures
  (list (make-binding 'car (tag-primitive car))
        (make-binding 'cdr (tag-primitive cdr))
        (make-binding 'cons (tag-primitive cons))
        (make-binding 'null? (tag-primitive null?))
        (make-binding '+ (tag-primitive +))
        (make-binding '* (tag-primitive *))
        (make-binding '/ (tag-primitive /))
        (make-binding '- (tag-primitive -))
        (make-binding 'list (tag-primitive list))
        (make-binding 'display (tag-primitive display))
        (make-binding 'newline (tag-primitive newline))
;;      more primitives
        ))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (evl input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))
;;; (driver-loop)