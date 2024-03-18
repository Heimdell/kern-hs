(def n 10)

(def new-line "
")

(def s new-line)

(def i 0)
(def j 0)
(def d 1)

(def row '(do
    (set! s (++ s "@"))
    (set! j (- j 1))
    (cond (> j 0) (eval row (get-env)) ())))

(def task '(do
    (cond (>= i n) (set! d (- 0 d)) ())
    (set! i (+ i d))
    (set! j i)
    (cond (> i 0) (do (eval row (get-env))
                      (set! s (++ s new-line))
                      (eval task (get-env)))
          (set! d 1))))

(eval task (get-env))

(print s)