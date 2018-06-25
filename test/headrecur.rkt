(define Y
  (lambda (h)
    ((lambda (x) (x x))
     (lambda (g)
       (h (lambda (t) ((g g) t)))))))

;; head-recursive factorial
(define fac
  (Y
   (lambda (f)
     (lambda (x)
       (if (< x 2)
           1
           (* x (f (- x 1))))))))

(fac 10)