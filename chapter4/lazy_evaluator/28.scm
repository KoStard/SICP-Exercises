; For apply we need to check if the procedure is primitive or compound
; For that we need to have the actual value of the procedure
; So if it's thunk, then it will fail if we don't force the value before passing to the apply
(load "./chapter4/lazy_evaluator/base.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (apply (eval (operator exp) env)   ; Changing this to not enforce the value before calling apply
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))



; This is working, because the test is not delayed value, is directly bound to a lambda
(run-all
    '(
        (define (test x) x)
        (display (test 10)) (newline)
    )
)


; This will fail, because the test will become a thuck, as it's argument of get-proc and hence the apply
; won't know how to execute that.
(run-all
    '(
        (define (get-proc f) f)
        (display ((get-proc test) 10)) (newline)
    )
)