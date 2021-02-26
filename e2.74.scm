;;; division personnel records (as one file)
;;; |> employee record
;;;   |> salary
;;;   |> address, ...

;;; Each division has to implement and register
;;; - find
;;; - belongs
;;; - get_salary

;;; They have to provide a way to understand whose division personnel records these are (maybe presenting the files as list of division name and records)


(define (find division_personnel_records f) 
    ((get 'find (division division_personnel_records))
        (records division_personnel_records f))
)

(define (belongs record employee division) 
    ((get 'belongs division)
        record employee)
)

(define (division division_personnel_records) (car division_personnel_records))
(define (records division_personnel_records) (cadr division_personnel_records))

(define (get_record employee division_personnel_records)
    (find division_personnel_records (lambda (record) (belongs record employee (division division_personnel_records))))
)

(define (get_salary employee_record division)
    ((get 'get_salary division) employee_record)
)

(define (find_employee_record employee all_division_records)
    (if (null? all_division_records) false
        (let ((result_in_first (get_record employee (car all_division_records))))
            (if (result_in_first)
                result_in_first
                (find_employee_record employee (cdr all_division_records))
            )
        )
    )
)


;;; When adding new company, nothing should be changed in the central system except installing the new division's package.