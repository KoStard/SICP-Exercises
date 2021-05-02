; Implement if-fail

(load "./chapter4/non-deterministic/ch4-ambeval.scm")

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-test exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))
(define (analyze-if-fail exp)
    (let ((test-proc (analyze (if-fail-test exp)))
        (alternative-proc (analyze (if-fail-alternative exp))))
        (lambda (env succeed fail)
            (test-proc
                env
                succeed
                (lambda ()
                    (alternative-proc
                        env
                        succeed
                        fail
                    )
                )
            )
        )
    )
)

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

         
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
        (list 'display display)
        (list 'newline newline)
        (list 'or internal-or)
        (list 'and internal-and)
        (list 'even? even?)
;;      more primitives
        ))

         
(define the-global-environment (setup-environment))

; (driver-loop)
(define (run-all seq)
  (define (internal-loop try-again seq)
    (if (not (null? seq))
        (let ((input (car seq)))
        (if (eq? input 'try-again)
            (try-again)
            (begin
                (newline)
                (display ";;; Starting a new problem ")
                (ambeval input
                        the-global-environment
                        ;; ambeval success
                        (lambda (val next-alternative)
                        (announce-output output-prompt)
                        (user-print val)
                        (internal-loop next-alternative (cdr seq)))
                        ;; ambeval failure
                        (lambda ()
                        (announce-output
                            ";;; There are no more values of")
                        (user-print input)
                        (run-all (cdr seq))))))))
    )
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (run-all (cdr seq))) seq))


(run-all '(
    (define (require p)
      (if (not p) (amb)))
    (if-fail (let ((x (amb 1 3 5 8)))
        (require (even? x))
        x)
        'all-odd
    )
    try-again  
))