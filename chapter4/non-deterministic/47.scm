(load "./chapter4/non-deterministic/sentences-base.scm")

(run-all '(
    (define (parse-verb-phrase)
        (amb (parse-word verbs)
            (list 'verb-phrase
                    (parse-verb-phrase)
                    (parse-prepositional-phrase))))

    (parse '(the student studies))
    try-again
))