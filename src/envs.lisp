
(do
  (def ctor (lambda (a)
    (do
      (def inc (lambda (b) (set! a (+ a b))))
      (def get (lambda () a))
      (lambda (msg . rest)
        (cond (= msg 'inc) (inc . rest)
          (cond (= msg 'get) (get . rest)
            "what?")))
    )))
  (do
    (def obj (ctor 42))
    (obj 'inc 5)
    (print (obj 'get) "\n")
    (obj 'inc 50)
    (print (obj 'get) "\n")
    (obj 'inc 500)
    (print (obj 'get) "\n"))
)

;; (def obj (ctor 42)
