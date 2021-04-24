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
    (define (distinct? l)
        (cond ((null? l) true)
            ((null? (cdr l)) true)
            ((member (car l) (cdr l)) false)
            (else (distinct? (cdr l)))
        )
    )
    (define (yachts)
      (let (
        (SB-yacht 'G)
        (MM-yacht 'L)
        (MH-yacht 'R)
        (CD-yacht 'M)
        (DP-yacht 'A)
        (SB (amb 'G 'L 'R 'M 'A))
        (MM (amb 'G 'L 'R 'M 'A))
        (MH (amb 'G 'L 'R 'M 'A))
        (CD (amb 'G 'L 'R 'M 'A))
        (DP (amb 'G 'L 'R 'M 'A))
      )
        (require (not (eq? SB-yacht SB)))
        (require (not (eq? MM-yacht MM)))
        (require (not (eq? MH-yacht MH)))
        (require (not (eq? CD-yacht CD)))
        (require (not (eq? DP-yacht DP)))
        (require (eq? SB 'M))
        (require (eq? MM 'A)) ; Comment this line to get the alternative solutions
        (require
          (cond 
            ((eq? SB 'G) (eq? SB-yacht DP))
            ((eq? MM 'G) (eq? MM-yacht DP))
            ((eq? MH 'G) (eq? MH-yacht DP))
            ((eq? CD 'G) (eq? CD-yacht DP))
            ((eq? DP 'G) (eq? DP-yacht DP))
          )
        )
        (require (distinct? (list SB MM MH CD DP)))
        ; (display (list SB MM MH CD DP)) (newline)
        (cond 
          ((eq? SB 'L) 'SB)
          ((eq? MM 'L) 'MM)
          ((eq? MH 'L) 'MH)
          ((eq? CD 'L) 'CD)
          ((eq? DP 'L) 'DP)
        )
      )
    )
    (yachts)
    try-again
))

