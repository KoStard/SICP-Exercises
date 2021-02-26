;;; Let's imagine we have N letters in the alphabet and M symbols in the message
;;; We are considering the case from e2.71.scm, with frequencies of 1, 2, 4, 8, .... 2**(n-1)
;;; For the most frequent symbols, the complexity of encoding one symbol will be O(N), because in the first node it will need to check 
;;; all records to find our in which branch it is located (if in any)
;;; In case of the lest frequent symbols, we have to check N records in the first node, then N-1 records in the second node, etc untill 
;;; reaching the end, so 1+2+3+...+N = N**2..., so the complexity here will be O(N**2)
;;; But this is also the least frequent message