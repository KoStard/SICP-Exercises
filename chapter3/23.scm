;;; Deque
;;; make-deque
;;; empty-deque?
;;; front-deque
;;; rear-deque
;;; front-insert-deque!
;;; rear-insert-deque!
;;; front-delete-deque!
;;; rear-delete-deque!

;;; deque items connector

(define (make-deque-connector)
    (cons '() '())
)

(define (front-connector-ptr connector)
    (car connector)
)

(define (rear-connector-ptr connector)
    (car connector)
)

(define (rear-connector-ptr connector)
    (cdr connector)
)

(define (set-front-connector-ptr! connector front-item)
    (set-car! connector front-item)
)

(define (set-rear-connector-ptr! connector rear-item)
    (set-cdr! connector rear-item)
)

;;; deque

(define (make-deque)
    (cons '() '())
)

(define (front-deque-ptr deque)
    (car deque)
)

(define (rear-deque-ptr deque)
    (cdr deque)
)

(define (set-front-deque-ptr! deque item)
    (set-car! deque item)
)

(define (set-rear-deque-ptr! deque item)
    (set-cdr! deque item)
)

(define (empty-deque? deque)
    (or (null? (front-deque-ptr deque)) (null? (rear-deque-ptr deque)))
)

(define (front-deque deque)
    (if (empty-deque? deque)
        (error "Called front-deque on empty deque")
        (car (front-deque-ptr deque))
    )
)

(define (rear-deque deque)
    (if (empty-deque? deque)
        (error "Called rear-deque on empty deque")
        (car (rear-deque-ptr deque))
    )
)

(define (front-insert-deque! deque value)
    (let ((item (cons value (make-deque-connector))))
        (set-rear-connector-ptr! (cdr item) (front-deque-ptr deque))
        (if (empty-deque? deque) 
            (set-rear-deque-ptr! deque item)
            (set-front-connector-ptr! (cdr (front-deque-ptr deque)) item)
        )
        (set-front-deque-ptr! deque item)
    )
)
(define (rear-insert-deque! deque value)
    (let ((item (cons value (make-deque-connector))))
        (set-front-connector-ptr! (cdr item) (rear-deque-ptr deque))
        (if (empty-deque? deque) 
            (set-front-deque-ptr! deque item)
            (set-rear-connector-ptr! (cdr (rear-deque-ptr deque)) item)
        )
        (set-rear-deque-ptr! deque item)
    )
)
(define (front-delete-deque! deque)
    (cond ((empty-deque? deque)
        (error "Called front-delete-deque! on empty deque"))
        (else 
            (set-front-deque-ptr! deque (rear-connector-ptr (cdr (front-deque-ptr deque))))
            (if (not (empty-deque? deque)) (set-front-connector-ptr! (cdr (front-deque-ptr deque)) '())))
    )
)
(define (rear-delete-deque! deque)
    (cond ((empty-deque? deque)
        (error "Called rear-delete-deque! on empty deque"))
        (else 
            (set-rear-deque-ptr! deque (front-connector-ptr (cdr (rear-deque-ptr deque))))
            (if (not (empty-deque? deque)) (set-rear-connector-ptr! (cdr (rear-deque-ptr deque)) '())))
    )
)

(let ((deque (make-deque)))
    (front-insert-deque! deque 'b)
    (front-insert-deque! deque 'a)
    (rear-insert-deque! deque 'c)
    (display (front-deque-ptr deque)) (newline)
    (rear-delete-deque! deque)
    (rear-delete-deque! deque)
    (front-delete-deque! deque)
    (display (front-deque-ptr deque)) (newline)
    (display (rear-deque-ptr deque)) (newline)
    (display (empty-deque? deque)) (newline)
)