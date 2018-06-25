(define Y (λ(b)((λ(f)(b(λ(x)((f f) x))))
                (λ(f)(b(λ(x)((f f) x)))))))
 
(define F
  (λ (f)
    (λ (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))
 
((Y F) 10)