
(def i 1)
(def i-max 100)
(def n 0)
(def d 0)

(def remove-d
  '(cond (= 0 (mod n d)) (do (set! n (/ n d))
                             (eval remove-d (get-env)))
         ()))
(def task
  '(cond (<= i i-max) (do (set! n i)
                          (set! d 2) (eval remove-d (get-env))
                          (set! d 3) (eval remove-d (get-env))
                          (set! d 5) (eval remove-d (get-env))
                          (cond (= n 1) (print i " ") ())
                          (set! i (+ 1 i))
                          (eval task (get-env)))
         (set! i 1)))

(eval task (get-env))