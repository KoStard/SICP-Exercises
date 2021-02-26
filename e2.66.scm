;;; Implement lookup for tree-structured set
;;; The key is numeric

(define (lookup given_key set)
    (if (null? set) false
        (let ((current_key (key (entry set))))
            (cond ((= given_key current_key) (entry set))
                ((< given_key current_key) (lookup given_key (left_branch set)))
                ((> given_key current_key) (lookup given_key (right_branch set)))
            )
        )
    )
)