(load "./chapter4/non-deterministic/ch4-ambeval.scm")

(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (permanent-assignment-variable exp) (cadr exp))
(define (permanent-assignment-value exp) (caddr exp))
(define (analyze-permanent-assignment exp)
    (let ((var (permanent-assignment-variable exp))
        (vproc (analyze (permanent-assignment-value exp)))
    )
        (lambda (env succeed fail)
            (vproc env 
                (lambda (val fail-value-usage)
                    (set-variable-value! var val env)
                    (succeed 'ok
                        fail-value-usage
                    )
                )
                fail
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
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


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
    (define count 0)
    (let ((x (amb 'a 'b 'c))
        (y (amb 'a 'b 'c)))
        (set! count (+ count 1))
        (require (not (eq? x y)))
        (list x y count)
    )
    try-again
))