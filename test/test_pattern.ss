(def (assert True) (__builtin_nop))
(def (assert_eq a b) (assert (== a b)))

(def
    (if 0     _ b) b
    (if []    _ b) b
    (if False _ b) b
    (if _     a _) a
)
(def (id a) a)

(assert_eq 2 (if 0 (id 1) (id 2)))
(assert_eq 1 (if 1 (id 1) (id 2)))

