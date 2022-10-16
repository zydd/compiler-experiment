(def (assert_eq a b) (assert (== a b)))

(def (id a) a)
(def
    (if 0     _ b) b
    (if []    _ b) b
    (if False _ b) b
    (if _     a _) a
)


(def (mul acc count a b)
    (if (< count b)
        '(mul '(+ acc a) '(+ count 1) a b)
        acc
))
(assert_eq 25 (mul 0 0 (id 5) (+ 2 3)))


(def (foldl func accum lst)
    (if lst
        '(foldl func '(func accum '(car lst)) '(cdr lst))
        accum
))
(assert_eq 6 (foldl + 0 [1 2 3]))


(def (sum acc count max)
    (if (<= count max)
        '(sum (+ acc count) (+ count 1) max)
        acc
))
(assert_eq 10 (sum 0 0 4))


(def (add a b) (+ a b))
(def (sum_list list) (foldl add 0 list))
(assert_eq 10 (sum_list [1 2 3 4]))


(def (inf_loop) (inf_loop))
(assert_eq 200 (+
    (if (> 1 2) '(inf_loop) 100)
    (if (< 1 2) 100 '(inf_loop))
))
