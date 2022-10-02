(def
    (sum acc 0)
        acc

    (sum acc max)
        (sum (+ acc max) (- max 1))
)

(print "sum: ")
(println (sum 0 (* 10 (* 1000 1000))))

(def (apply f v) (f v))
(apply println "OK!")