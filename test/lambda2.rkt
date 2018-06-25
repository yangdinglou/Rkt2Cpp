(define t
  ((lambda (x) (x x))
   (lambda (factgen)
     (lambda (n)
       (if (= n 0)
           1
           (* n ((factgen factgen) (- n 1))))))))
(t 10)