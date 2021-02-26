(load "./e2.68.scm")

(define (generate_huffman_tree pairs)
    (successive_merge (make_leaf_set pairs))
)

(define (successive_merge elements)
    (if (null? (cdr elements))
        (car elements)
        (successive_merge (adjoin_set (make_code_tree (car elements) (cadr elements)) (cddr elements)))
    )
)

;;; (let ((tree (generate_huffman_tree (list '(A 4) '(B 2) '(C 1) '(D 1)))))
;;;     (display (encode '(A B C D) tree))
;;; )
