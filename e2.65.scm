;;; The complexity has to be O(N)
;;; We have function for converting tree to ordered list with O(N) complexity
;;; We have functions for doing union/intersection on ordered lists with O(N) complexity
;;; We have functions for converting ordered list to balanced tree with O(N) complexity
;;; So by combining these we will have the solutions with O(N) complexity

(load "./e2.64.scm") ; Getting function to convert list to balanced tree
(load "./e2.63.scm") ; Tree to list

(define (intersection_ordered_lists set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x (car set1)) (y (car set2)))
            (cond 
                ((= x y) (cons x (intersection_ordered_lists (cdr set1) (cdr set2))))
                ((> x y) (intersection_ordered_lists set1 (cdr set2)))
                ((< x y) (intersection_ordered_lists (cdr set1) set2))
            )
        )
    )
)

(define (union_ordered_lists set1 set2)
    (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
            (let ((x (car set1))
                (y (car set2))
            )
                (cond 
                    ((= x y) (cons x (union_ordered_lists (cdr set1) (cdr set2))))
                    ((> x y) (cons y (union_ordered_lists set1 (cdr set2))))
                    ((< x y) (cons x (union_ordered_lists (cdr set1) set2)))
                )
            )
        )
    )
)

(define (union_set set1 set2)
    (list_to_tree (union_ordered_lists (tree_to_list_2 set1) (tree_to_list_2 set2)))
)
(define (intersection_set set1 set2)
    (list_to_tree (intersection_ordered_lists (tree_to_list_2 set1) (tree_to_list_2 set2)))
)


;;; (display (tree_to_list_2 (union_set (list_to_tree (list 1 2 3 4 5)) (list_to_tree (list 5 6 7 8 9))))) (newline)
;;; (display (tree_to_list_2 (intersection_set (list_to_tree (list 1 2 3 4 5)) (list_to_tree (list 3 4 5 6 7 8 9))))) (newline)