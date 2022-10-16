(def (assert_eq a b) (assert (== a b)))

(assert_eq [3] (cons 3 []))
(assert_eq [2 3] (cons 2 (cons 3 [])))
(assert_eq [1 2 3] (cons 1 (cons 2 (cons 3 []))))

(cons 1 (cons 2 (cons 3 [])))
