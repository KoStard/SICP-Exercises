;;; Make a tree structure that will handle custom types (e.g. pair) as values and comparison methods

(define (entry tree) (car tree))
(define (left_branch tree) (cadr tree))
(define (right_branch tree) (caddr tree))
(define (make_tree entry left right)
    (list entry left right)
)

(define (adjoin_tree x tree comparator)
    (cond ((null? tree) (make_tree x '() '()))
        (else 
            (let ((comp-value (comparator x (entry tree))))
                (cond 
                    ((= comp-value 0) tree)
                    ((= comp-value -1)
                        (make_tree (entry tree)
                            (adjoin_tree x (left_branch tree) comparator)
                            (right_branch tree)
                        )
                    )
                    ((= comp-value 1)
                        (make_tree (entry tree)
                            (left_branch tree)
                            (adjoin_tree x (right_branch tree) comparator)
                        )
                    )
                    (else (error "The comparator returned invalid value - valid values are -1, 0, 1" (list x (entry tree) comp-value)))
                )
            )
        )
    )
)

(define (add_all_from_list_to_set l set)
    (if (null? l)
        set
        (add_all_from_list_to_set (cdr l) (adjoin_tree (car l) set))
    )
)

(define (get-from-tree x tree comparator)
    (cond ((null? tree) false)
        (else 
            (let ((comp-value (comparator (list x) (entry tree))))
                (cond 
                    ((= comp-value 0) (entry tree))
                    ((= comp-value -1) (get-from-tree x (left_branch tree) comparator))
                    ((= comp-value 1) (get-from-tree x (right_branch tree) comparator))
                    (else (error "The comparator returned invalid value - valid values are -1, 0, 1" (list x (entry tree) comp-value)))
                )
            )
        )
    )
)

(define (make-tree-object comparator)
    (let ((tree '()))
        (lambda (message)
            (cond 
                ((eq? message 'get) (lambda (x) (get-from-tree x tree comparator)))
                ((eq? message 'put) (lambda (x) (set! tree (adjoin_tree x tree comparator))))
            )
        )
    )
)

;;; (let ((tree (make-tree-object (lambda (a b) (cond ((< a b) -1) ((> a b) 1) (else 0))))))
;;;     (display ((tree 'get) 4)) (newline)
;;;     ((tree 'put) 4)
;;;     (display ((tree 'get) 4)) (newline)
;;;     ((tree 'put) 2)
;;;     (display ((tree 'get) 2)) (newline)
;;;     (display ((tree 'get) 4)) (newline)
;;; )