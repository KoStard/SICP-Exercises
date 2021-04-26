(load "./chapter4/non-deterministic/sentences-base.scm")

(run-all '(
    (parse '(the professor lectures to the student in the class with the cat))
    try-again
))
