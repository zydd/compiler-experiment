(def (strict !a) a)
(def (deferred 'a) a)

(strict '(+ 2 2))
(deferred (+ 2 2))
