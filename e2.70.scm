(load "./e2.69.scm")

(let ((tree (generate_huffman_tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))))
    (display (encode '(GET A JOB ; 3
    SHA NA NA NA NA NA NA NA NA ; 9
    GET A JOB ; 3
    SHA NA NA NA NA NA NA NA NA ; 9
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP ; 10
    SHA BOOM) tree)) (newline) ; 2
)

;;; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1) - 84 BITS
;;; If we used fixed length code, for each symbol we would need 3 bits and we have 33 symbols, then we would need at least 99 bits
;;; So this algorithm reduces the size by 17.86%