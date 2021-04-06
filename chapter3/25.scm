;;; Multi-dimensional tables just have other tables under their keys, so this is recursive pattern

(define (lookup keys table)
    (let ((first-key (car keys))
        (remaining-keys (cdr keys)))
        (let ((record (assoc first-key (cdr table))))
            (if record
                (if (not (null? remaining-keys))
                    (if (pair? (cdr record))
                        ;;; This is not perfect, as if saving pairs in the table we may get problems
                        (lookup remaining-keys record)
                        false
                    )
                    (cdr record)
                )
                false))
    )
)

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! keys value table)
    (let ((first-key (car keys))
        (remaining-keys (cdr keys)))
        (let ((record (assoc first-key (cdr table))))
            (if (not (null? remaining-keys))
                (if record
                    (insert! remaining-keys value record)
                    (let ((new-record (cons (list first-key) (cdr table))))
                        (set-cdr! table new-record)
                        (insert! remaining-keys value (car new-record))
                    )
                )
                ;;; 1D table left
                (if record
                    (set-cdr! record value)
                    (set-cdr! table
                        (cons (cons first-key value) (cdr table)))
                )
            )
        )
    )
'ok)

(define (make-table)
  (list '*table*))

;;; (let ((table (make-table)))
;;;     (insert! '(a b c) 3 table)
;;;     (insert! '(a c) 2 table)
;;;     (display (lookup '(a c) table))
;;;     ;;; (display (assoc 'a (cdr table)))
;;; )