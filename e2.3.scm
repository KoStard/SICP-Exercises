; rectangles
; goal -> abstraction barriers
; get_side_length - for working with the rectangle

; loading point procedures
(load "./e2.2.scm")

(define (get_perimeter rectangle)
    (* (+ (get_height_length rectangle) (get_width_length rectangle)) 2)
)

(define (get_area rectangle)
    (* (get_height_length rectangle) (get_width_length rectangle))
)

; ============ abstraction barrier ============

(define (get_height_length rectangle)
    (length (get_height rectangle))
)

(define (get_width_length rectangle)
    (length (get_width rectangle))
)

(define (print_rectangle rectangle)
    (display "Width")
    (newline)
    (print_segment (get_width rectangle))
    (display "Height")
    (newline)
    (print_segment (get_height rectangle))
)
; ============ abstraction barrier ============

; (define (get_height rectangle)
;     (cdr rectangle)
; )
;
; (define (get_width rectangle)
;     (car rectangle)
; )
;
; ; p1 - p2 - p3 this should be the sequence of edges
; (define (make_rectangle p1 p2 p3)
;     (let ((s1 (make_segment p1 p2))
;         (s2 (make_segment p2 p3)))
;         (let (
;             (l1 (length s1))
;             (l2 (length s2))
;             )
;             ; putting width, height
;             (if (> l1 l2)
;                 (cons s1 s2)
;                 (cons s2 s1)
;             )
;         )
;     )
; )


; ============ abstraction barrier ============


(define (get_height rectangle)
    (let ((s1 (car rectangle))
        (s2 (cdr rectangle)))
        (let (
            (l1 (length s1))
            (l2 (length s2))
            )
            ; putting the smaller is the height
            (if (> l1 l2)
                s2
                s1
            )
        )
    )
)

(define (get_width rectangle)
    (let ((s1 (car rectangle))
        (s2 (cdr rectangle)))
        (let (
            (l1 (length s1))
            (l2 (length s2))
            )
            ; putting the bigger is the width
            (if (> l1 l2)
                s1
                s2
            )
        )
    )
)

; p1 - p2 - p3 this should be the sequence of edges
(define (make_rectangle p1 p2 p3)
    (let ((s1 (make_segment p1 p2))
        (s2 (make_segment p2 p3)))
        (cons s1 s2)
    )
)


(let (
    (rectangle (make_rectangle (make_point 1 1) (make_point 1 4) (make_point 6 4) ))
    )
    (print_rectangle rectangle)
    (display "Perimeter: ")
    (display (get_perimeter rectangle))
    (newline)
    (display "Area: ")
    (display (get_area rectangle))
    (newline)
    )
