(define (f a b)
  (if (and (= a b) (> a 0))
      (begin
        (set! a (+ a 1))
        a)
      (begin
        (set! b (- b 1))
        b)))
(f 0 0)
(f 1 0)
(f 1 1)
(f 0 1)