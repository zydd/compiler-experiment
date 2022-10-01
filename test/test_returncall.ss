(def (assert True) (__builtin_Nop))
(def (assert_eq a b) (assert (== a b)))


(def
    (eat_all a) eat_all
)

(assert_eq eat_all (eat_all 1 2 3 4 5 6))


(def (eat_1 a) "OK")
(def (eat_2 a) eat_1)
(def (eat_3 a) eat_2)
(def (eat_4 a) eat_3)
(def (eat_t a) (eat_all a))

(assert_eq "OK" (eat_4 1 2 3 4))


(def (eat_at_least_3 a b c) (eat_all a b c))

(assert_eq eat_all (eat_at_least_3 1 2 3 4 5 6))
