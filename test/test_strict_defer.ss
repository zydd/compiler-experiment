(def (strict !a) a)
(def (deferred 'a) a)

(def
    (non_strict_match 0 b) 333
    (non_strict_match a b) b
)

(def
    (strict_match 0 !b) 333
    (strict_match a !b) b
)


(assert (not (is_deferred (strict '(+ 222 222)))))
(strict_match 0 '(+ 222 222))

(assert (is_deferred (non_strict_match 1 '(+ 222 222))))
(assert (not (is_deferred (strict_match 1 '(+ 222 222)))))

; (assert (is_deferred (deferred (+ 222 222))))
