(def (assert True) (__builtin_nop))
(def (assert_eq a b) (assert (== a b)))


(def
    (fold func accum [])
        accum

    (fold func accum list)
        (fold func (+ accum (car list)) (cdr list))
)

(assert_eq 10 (fold + 0 [1 2 3 4]))


(def (sum_formula n) (/ (* n (+ n 1)) 2))

(def
    (sum acc 0)
        acc

    (sum acc max)
        (sum (+ acc max) (- max 1))
)

(def (assert_sum n) (assert_eq (sum_formula n) (sum 0 n)))
(assert_sum 100)


(def
    (foldr func accum [])
        accum

    (foldr func accum lst)
        (func (car lst) '(foldr func accum (cdr lst)))
)
(assert_eq 10 (foldr + 0 [1 2 3 4]))

(def
    (foldl func accum [])
        accum

    (foldl func accum lst)
        '(foldl func (func accum (car lst)) (cdr lst))
)
(assert_eq 6 (foldl + 0 [1 2 3]))
