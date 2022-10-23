(assert (eq [() () ()]
    (seq
        (println 1)
        (println 2)
        (println 3)
    )
))

(assert (== [() () ()]
    (seq
        '(println 4)
        '(println 5)
        '(println 6)
    )
))

; tail call
(def (f) (seq 3 4))
(def (g) (seq 3 4 (f)))

(assert (== [4 3] (f)))
(assert (== (cons [4 3] [4 3]) (g)))
