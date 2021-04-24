(load "./chapter4/non-deterministic/ch4-ambeval.scm")

(define the-global-environment (setup-environment))

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
))