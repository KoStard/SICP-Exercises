;;; Data directed design of the eval
;;; This will also allow us to extend the language later

;;; Loading multidimensional table implementation
(load "./chapter3/25.scm")
(load "./chapter4/analysing_evaluator/commons.scm")
(load "./chapter4/analysing_evaluator/begin.scm")
(load "./chapter4/analysing_evaluator/assignment.scm")
(load "./chapter4/analysing_evaluator/cond.scm")
(load "./chapter4/analysing_evaluator/definition.scm")
(load "./chapter4/analysing_evaluator/if.scm")
(load "./chapter4/analysing_evaluator/lambda.scm")
(load "./chapter4/analysing_evaluator/quoted.scm")
(load "./chapter4/analysing_evaluator/environment.scm")

(define (evl exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
            (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((and (get-checker (get-tag exp)) ((get-checker (get-tag exp)) exp))
            ((get-analyzer (get-tag exp)) exp)
        )
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))
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

(define the-global-environment (setup-environment))
(display (evl '(cond (true 1)) the-global-environment)) (newline)