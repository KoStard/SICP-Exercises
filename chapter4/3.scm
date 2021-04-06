;;; Data directed design of the eval
;;; This will also allow us to extend the language later

;;; Loading multidimensional table implementation
(load "./chapter3/25.scm")

(define bindings (make-table))

;;; For every tag we should have checker and evaluator
(define (put tag proc-name implementation)
    (insert! (list tag proc-name) implementation bindings)
    'ok
)

(define (put-checker tag implementation)
    (put tag 'checker implementation)
)

(define (put-evaluator tag implementation)
    (put tag 'evaluator implementation)
)

(define (get tag proc-name)
    (lookup (list tag proc-name) bindings)
)

(define (get-checker tag)
    (if (symbol? tag) 
        (get tag 'checker)
        false
    )
)

(define (get-evaluator tag)
    (get tag 'evaluator)
)

(define (get-tag exp) (car exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (evl exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;;; ((quoted? exp) (text-of-quotation exp))
        ;;; ((assignment? exp) (eval-assignment exp env))
        ;;; ((definition? exp) (eval-definition exp env))
        ;;; ((if? exp) (eval-if exp env))
        ;;; ((lambda? exp)
        ;;;  (make-procedure (lambda-parameters exp)
        ;;;                  (lambda-body exp)
        ;;;                  env))
        ;;; ((begin? exp) 
        ;;;  (eval-sequence (begin-actions exp) env))
        ;;; ((cond? exp) (evl (cond->if exp) env))
        ((and (get-checker (get-tag exp)) ((get-checker (get-tag exp)) exp))
            ((get-evaluator (get-tag exp)) exp env)
        )
        ((application? exp)
         (appl (evl (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (install-quoted) 
    (define tag 'quote)
    (define (quoted? exp)
        (tagged-list? exp tag))

    (define (text-of-quotation exp env) (cadr exp))

    (put-checker tag quoted?)
    (put-evaluator tag text-of-quotation)
)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (install-assignment) 
    (define tag 'set!)
    (define (assignment? exp)
        (tagged-list? exp tag))
    (define (assignment-variable exp) (cadr exp))
    (define (assignment-value exp) (caddr exp))
    (define (make-assignment var val) (list tag var val))

    (define (eval-assignment exp env)
        (set-variable-value! (assignment-variable exp)
                            (evl (assignment-value exp) env)
                            env)
        'ok)

    (put-checker tag assignment?)
    (put-evaluator tag eval-assignment)
    (put tag 'make-assignment make-assignment)
)

(define (make-assignment . args) (apply (get 'set! 'make-assignment) args))

(define (install-definition) 
    (define tag 'define)
    (define (definition? exp)
        (tagged-list? exp tag))
    (define (definition-variable exp)
        (if (symbol? (cadr exp))
            (cadr exp)
            (caadr exp)))
    (define (definition-value exp)
        (if (symbol? (cadr exp))
            (caddr exp)
            (make-lambda (cdadr exp)   ; formal parameters
                        (cddr exp)))) ; body

    (define (eval-definition exp env)
        (define-variable! (definition-variable exp)
                            (evl (definition-value exp) env)
                            env)
        'ok)

    (define (make-define variable value)
        (list tag variable value)
    )

    (put-checker tag definition?)
    (put-evaluator tag eval-definition)
    (put tag 'make make-define)
    (put tag 'definition-variable definition-variable)
    (put tag 'definition-value definition-value)
)

(define (make-define . args)
    (apply (get 'define 'make) args)
)

(define (definition-variable . args) (apply (get 'define 'definition-variable) args))
(define (definition-value . args) (apply (get 'define 'definition-value) args))

(define (install-if) 
    (define tag 'if)
    (define (if? exp) (tagged-list? exp tag))
    (define (if-predicate exp) (cadr exp))
    (define (if-consequent exp) (caddr exp))
    (define (if-alternative exp)
        (if (not (null? (cdddr exp)))
            (cadddr exp)
            'false))
    (define (eval-if exp env)
        (if (true? (evl (if-predicate exp) env))
            (evl (if-consequent exp) env)
            (evl (if-alternative exp) env)))

    (define (make-if predicate consequent alternative)
        (list 'if predicate consequent alternative))


    (put-checker tag if?)
    (put-evaluator tag eval-if)
    (put tag 'make-if make-if)
)

(define (make-if . args)
    (apply (get 'if 'make-if) args)
)

(define (install-lambda) 
    (define tag 'lambda)
    (define (lambda? exp) (tagged-list? exp 'lambda))
    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))

    (define (make-lambda parameters body)
        (cons 'lambda (cons parameters body)))

    (define (eval-lambda exp env)
        (make-procedure (lambda-parameters exp)
                        (lambda-body exp)
                        env)
    )

    (put-checker tag lambda?)
    (put-evaluator tag eval-lambda)
    (put tag 'make-lambda make-lambda)
)

(define (make-lambda . args)
    (apply (get 'lambda 'make-lambda) args)
)

(define (install-begin) 
    (define tag 'begin)

    (define (begin? exp) (tagged-list? exp tag))
    (define (begin-actions exp) (cdr exp))
    (define (last-exp? seq) (null? (cdr seq)))
    (define (first-exp seq) (car seq))
    (define (rest-exps seq) (cdr seq))
    (define (sequence->exp seq)
        (cond ((null? seq) seq)
                ((last-exp? seq) (first-exp seq))
                (else (make-begin seq))))
    (define (make-begin seq) (cons tag seq))

    (define (eval-sequence exps env)
        (cond   ((null? exps) '())
                ((last-exp? exps) (evl (first-exp exps) env))
                (else (evl (first-exp exps) env)
                    (eval-sequence (rest-exps exps) env))))
    
    (define (eval-begin exp env)
        (eval-sequence (begin-actions exp) env)
    )

    (put-checker tag begin?)
    (put-evaluator tag eval-begin)
    (put tag 'make-begin make-begin)
    (put tag 'eval-sequence eval-sequence)
    (put tag 'sequence->exp sequence->exp)
)

(define (make-begin . args) (apply (get 'begin 'make-begin) args))
(define (eval-sequence . args) (apply (get 'begin 'eval-sequence) args))
(define (sequence->exp . args) (apply (get 'begin 'sequence->exp) args))

(define (install-cond) 
    (define tag 'cond)

    (define (cond? exp) (tagged-list? exp 'cond))
    (define (cond-clauses exp) (cdr exp))
    (define (cond-else-clause? clause)
        (eq? (cond-predicate clause) 'else))
    (define (cond-predicate clause) (car clause))
    (define (cond-actions clause) (cdr clause))
    (define (cond->if exp)
        (expand-clauses (cond-clauses exp)))

    (define (expand-clauses clauses)
        (if (null? clauses)
            'false                          ; no else clause
            (let ((first (car clauses))
                    (rest (cdr clauses)))
                (if (cond-else-clause? first)
                    (if (null? rest)
                        (sequence->exp (cond-actions first))
                        (error "ELSE clause isn't last -- COND->IF"
                            clauses))
                    (make-if (cond-predicate first)
                            (sequence->exp (cond-actions first))
                            (expand-clauses rest))))))

    (define (eval-cond exp env) (evl (cond->if exp) env))

    (put-checker tag cond?)
    (put-evaluator tag eval-cond)
)

;;; (define (install-)
;;;     (define tag ')
;;;     (put-checker tag )
;;;     (put-evaluator tag )
;;; )


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(install-quoted)
(install-assignment)
(install-definition)
(install-if)
(install-lambda)
(install-begin)
(install-cond)
(install-assignment)

(define (zip a b aggregator)
    (cond 
        ((and (null? a) (null? b)) '())
        ((or (null? a) (null? b)) (error "Mismatch of number of arguments -- ZIP" a b))
        (else (cons (aggregator (car a) (car b)) (zip (cdr a) (cdr b) aggregator)))
    )
)

(define (appl procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (zip (procedure-parameters procedure)
                arguments
                make-binding)
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (evl (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


;;; This way anyone can add installable modules for the programming language
;;; Was not executed, as currently some procedures are still missing, so some problems may still be present