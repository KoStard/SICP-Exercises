;;; This function is generating the trees from left to right.
;;; Starting with a list of N elements, it's splitting it to (N-1)/2, 1, (N-(N-1)/2-1) elements
;;; This way we are making sure that the maximal difference of sizes between right and left subtrees will be 1
;;; But we can't just split the list easily, as this will require to iterate through all elements just for doing that, 
;;; which would make the process slower
;;; Instead of that we are splitting the array while also processing the values. That's why we have the remaining elements in the response
;;; as that already has removed the used elements.
;;; Also there is no need to control the left pivot too, only the right one is enough, as the elements are being removed from the left and when
;;; the partial_tree function gets elements list, it know that the first n elements are for him (even when we call this for the right branch)
;;; Let's call "pt" the "partial_tree" function for convenience.
;;; pt(N)
;;;     call pt((N-1)/2), get the tree and the non-left elements
;;;     get the first of the non-left elements for the current node
;;;     call pt(N-(N-1)/2-1) for getting the right tree and the remaining elements, which will be used by the parent node
;;; 
;;;     if the n is 0, that means that this is an empty node, so just return empty list and all elements with it
;;;
;;; This also explains why the list has to be ordered.
;;; 
;;; Let's draw the process for (1 3 5 7 9 11).
;;; pt(6)
;;;     pt(2)
;;;         pt(0) -> '() (1 3 5 7 9 11)
;;;         1
;;;         pt(1) -> '(3) (5 7 9 11)
;;;     5
;;;     pt(3)
;;;         pt(1) -> '(7) (9 11)
;;;         9
;;;         pt(1) -> '(11) '()
;;; 
;;; 
;;; So the tree will be 
;;;      -------5-------
;;;     1----       ----9----
;;;         3       7       11
;;; 

(load "./e2.63.scm")


(define (list_to_tree elements)
    (car (partial_tree elements (length elements)))
)


(define (partial_tree elements n)
    (if (= n 0)
        (cons '() elements)
        (let ((left_size (quotient (- n 1) 2)))
            (let ((left_result (partial_tree elements left_size)))
                (let ((left_tree (car left_result))
                        (non_left_elements (cdr left_result)))
                    (let ((current_element (car non_left_elements))
                        (right_size (- n 1 left_size))
                        (right_elements (cdr non_left_elements)))
                        (let ((right_result (partial_tree right_elements right_size)))
                            (let ((right_tree (car right_result))
                                (remaining_elements (cdr right_result)))
                                (cons (make_tree current_element left_tree right_tree) remaining_elements)
                            )
                        )
                    )
                )
            )
        )
    )
)


;;; Getting what we expect to get
;;; (display (list_to_tree (list 1 3 5 7 9 11)))


;;; Now, calculating the complexity
;;; Because we are going through each element only once and we are removing from the beginning of the list one by one,
;;; it's O(N)
