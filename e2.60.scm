;;; Representing sets as lists where duplicates are allowed
;;; This only changes the representation, but the expectations from the sets are the same (as it's called set)
;;; These are the expectations:
;;; 1 - for any S and any object x (element_of_set? x (adjoin_set x S)) is true
;;; 2 - for any sets S and T and any object x, (element_of_set? x (union_set S T)) = (or (element_of_set? x S) (element_of_set? x T))
;;; 3 - for any sets S and T and any object x, (element_of_set? x (intersect_set S T)) = (and (element_of_set? x S) (element_of_set? x T))
;;; 4 - for any object x, (element_of_set? x '()) is false

;;; So from this perspective, the property of unique elements is not coming from the definition of the sets, so there can be representations with duplicate elements too.
;;; Also it seems like even regular lists fit the requirements.

(load "./e2.59.scm")

(define (element_of_set? x set)
    (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element_of_set? x (cdr set)))
    )
)

(define (adjoin_set x set)
    (cons x set)
)

(define (intersection_set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
        ((element_of_set? (car set1) set2)
            (cons (car set1) (intersection_set (cdr set1) set2))
        )
        (else (intersection_set (cdr set1) set2))
    )
)

(define (union_set set1 set2)
    (if (null? set2)
        set1
        (union_set (adjoin_set (car set2) set1) (cdr set2))
    )
)


;;; With this implementation we remove the dependency on the element_of_set? from the adjoin_set and union_set, which means that there are now
;;; much faster. But in opposite, as there are more (duplicate) elements in the set, the element_of_set? and hence the intersection_set are slower.
;;; The complexities:
;;; adjoin_set - O(1) vs O(N)
;;; union_set - O(N) vs O(N*N)
;;; element_of_set - O(N*D) vs O(N)
;;; intersection_set - O(N**2*D**2) vs O(N**2)
;;; 
;;; N - the number of unique elements
;;; D - the average number of duplicates of items
;;; 
;;; So from here it obvious that in cases when the adjoin_set and the union_set are used much more often than teh element_of_set? and the intersection_set this will be more useful than the one with deduplication.


;;; (display (union_set '(1 2 3 4) '(3 4 5 6)))
