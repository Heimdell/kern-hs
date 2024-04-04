(do
  (def eval (macro (a)
    (eval-in a (get-env))))

  (def and1 (macro (a b)
    (cond a b #false)))

  (def and2 (macro (a b)
    (cond a (eval b) #false)))

  (def and3 (macro (a b)
    (cond a (eval (eval b)) #false)))

  (def and4 (macro (a b)
    (cond a (eval (eval (eval b))) #false)))

  (def or (macro (a b)
    (cond a #true (eval b))))
)