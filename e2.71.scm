;;; n symbols
;;; the relative frequencies 1, 2, 4, ... 2**(n-1)
;;; 1 - scretch the tree for n=5
;;; 2 - scretch the tree for n=10
;;; 3 - how many bits required to encode most frequent item?
;;; 4 - how many bits required to encode least frequent item?

;;; What does the 1, 2, 4, ..., 2**(n-1) mean?
;;; It means that sum(a1, a2, a3, ..., a(i-1)) < ai
;;; This is important, as when creating Hoffman trees we are always getting 2 nodes with smallest weights.
;;; This means that records with lowest i will be merged in every step. And hence the last element will be merged the last.
;;; So it will always have 1 bit.

;;; Let's draw the tree for n=5
;;;             ()
;;; a5                      ()
;;;                 a4              ()
;;;                         a3              ()
;;;                                 a2              a1

;;; This structure also means that it won't be balanced at all. By fact, it will be the least balanced structure.
;;; The first element in the list will have n-1 bits.

;;; Let's draw the tree for n=10
;;;             ()
;;; a10                      ()
;;;                 a9              ()
;;;                         a8              ()
;;;                                 a7              ()
;;;                                           a6            ()
;;;                                                   a5            ()
;;;                                                         a4              ()
;;;                                                                 a3              ()
;;;                                                                            a2        a1


;;; Test
(load "./e2.69.scm")

(display (generate_huffman_tree '((a1 1) (a2 2) (a3 4) (a4 8) (a5 16)))) (newline)