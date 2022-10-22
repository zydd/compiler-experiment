(def (assert_eq a b) (assert (== a b)))

(def
    (_if 0     _ b) b
    (_if []    _ b) b
    (_if False _ b) b
    (_if _     a _) a
)
(def (id a) a)

(assert_eq 2 (_if 0 (id 1) (id 2)))
(assert_eq 1 (_if 1 (id 1) (id 2)))

