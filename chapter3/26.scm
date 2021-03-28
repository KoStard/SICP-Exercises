(load "./chapter3/26.abstract-tree.scm")

(define (lookup keys table)
    (let ((first-key (car keys))
        (remaining-keys (cdr keys)))
        ;;; (let ((record (((cdr table) 'get) first-key)))
        (let ((record (assoc-custom first-key (cdr table))))
            (if record
                (if (not (null? remaining-keys))
                    (if (procedure? (cdr record))
                        (lookup remaining-keys record)
                        false
                    )
                    (cdr record)
                )
                false))
    )
)

(define (assoc-custom key records)
    ((records 'get) key)
)

(define (insert! keys value table comparator)
    (let ((first-key (car keys))
        (remaining-keys (cdr keys)))
        (let ((record (assoc-custom first-key (cdr table))))
            (if (not (null? remaining-keys))
                (if record
                    (insert! remaining-keys value record comparator)
                    (begin
                        (let ((new-entry (cons first-key (make-tree-object comparator))))
                            (((cdr table) 'put) new-entry)
                            (insert! remaining-keys value new-entry comparator)
                        )
                    )
                )
                ;;; 1D table left
                (if record
                    (set-cdr! record value)
                    (((cdr table) 'put) (cons first-key value))
                )
            )
        )
    )
'ok)

(define (make-table comparator)
  (cons '*table* (make-tree-object comparator)))

(let ((table (make-table (lambda (x y) (cond ((< (car x) (car y)) -1) ((= (car x) (car y)) 0) ((> (car x) (car y)) 1) ))))
    (comparator (lambda (x y) (cond ((< (car x) (car y)) -1) ((= (car x) (car y)) 0) ((> (car x) (car y)) 1) )))
)
    (insert! '(1 2 3) 'a table comparator)
    (insert! '(1 3) 'b table comparator)
    (display (lookup '(1 3) table))
    ;;; (display (cdr table))
    ;;; (display (assoc-custom 'a (cdr table)))
)