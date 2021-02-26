(define (entry tree) (car tree))
(define (left_branch tree) (cadr tree))
(define (right_branch tree) (caddr tree))
(define (make_tree entry left right)
    (list entry left right)
)

(define (adjoin_set x set)
    (cond ((null? set) (make_tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
            (make_tree (entry set)
                (adjoin_set x (left_branch set))
                (right_branch set)
            )
        )
        ((> x (entry set))
            (make_tree (entry set)
                (left_branch set)
                (adjoin_set x (right_branch set))
            )
        )
    )
)

(define (add_all_from_list_to_set l set)
    (if (null? l)
        set
        (add_all_from_list_to_set (cdr l) (adjoin_set (car l) set))
    )
)

;;; The problem

(define (tree_to_list_1 tree)
    (if (null? tree)
        '()
        (append (tree_to_list_1 (left_branch tree))
            (cons (entry tree) (tree_to_list_1 (right_branch tree))))
    )
)

(define (tree_to_list_2 tree)
    (define (copy_to_list tree result_list)
        (if (null? tree)
            result_list
            (copy_to_list (left_branch tree)
                (cons (entry tree) (copy_to_list (right_branch tree) result_list))
            )
        )
    )
    (copy_to_list tree '())
)

;;; Same
;;; (let 
;;;     ((tree (make_tree 7 (make_tree 3 (make_tree 1 '() '()) (make_tree 5 '() '())) (make_tree 9 '() (make_tree 11 '() '())) ) ))
;;;     (display (tree_to_list_1 tree)) (newline)
;;;     (display (tree_to_list_2 tree)) (newline)
;;; )

;;; Same
;;; (let 
;;;     ((tree (make_tree 3 (make_tree 1 '() '()) (make_tree 7 (make_tree 5 '() '()) (make_tree 9 '() (make_tree 11 '() '() )) )) ))
;;;     (display (tree_to_list_1 tree)) (newline)
;;;     (display (tree_to_list_2 tree)) (newline)
;;; )

;;; (let 
;;;     ((tree (make_tree 5 (make_tree 3 (make_tree 1 '() '()) '() ) (make_tree 9 (make_tree 7 '() '()) (make_tree 11 '() '())) )))
;;;     (display (tree_to_list_1 tree)) (newline)
;;;     (display (tree_to_list_2 tree)) (newline)
;;; )