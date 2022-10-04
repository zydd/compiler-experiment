(def (assert True) (__builtin_nop))
(def (assert_eq a b) (assert (== a b)))


(def (>> a b) b)
(def (<< a b) a)

(def
    (putchars_rev [])
        ()

    (putchars_rev chars)
        (>> (putchar (car chars)) (putchars_rev (cdr chars))))

(def
    (putchars [])
        ()

    (putchars chars)
        (<< (putchars (cdr chars)) (putchar (car chars))))

(def
    (gen acc 0)
        []

    (gen acc n)
        (cons (+ 65 acc) (gen (+ acc 1) (- n 1))))

(putchars (gen 0 26))
(putchar 10)
(putchars_rev (gen 0 26))
(putchar 10)

(def (pick a b c d e) a)

(assert_eq 1 (pick 1 2 3 4 5))
