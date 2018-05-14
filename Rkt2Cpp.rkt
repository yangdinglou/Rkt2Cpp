#lang racket
(define main-list (list))

(define (deduce exp) 0)
(define (gen-symbol exp) 0)
(define (gen-parm exp)
  (let ((ret ""))
    (if (= (length exp) 1)
        (string-replace ret ret (string-append ret (if (symbol? (car exp))
                                            (symbol->string (car exp))
                                            (number->string (car exp))) ")"))
        (string-replace ret ret (string-append ret
                                        (if (symbol? (car exp))
                                            (symbol->string (car exp))
                                            (number->string (car exp)))
                                        ", "
                                        (gen-parm (cdr exp)))))))
        
(define (constructor-run exp)
  (list (string-append "[] {"
                        (symbol->string (car exp))
                        " tmp("
                        (gen-parm (cdr exp))
                        ";"
                        "return tmp();"
                        "}();")))
(define (gen-exec exp)
  (if (pair? exp)
      (constructor-run exp)
      (list (string-append (symbol->string exp) "();"))))
    
;todo


(define (emit line)
  (display line)
  (newline))
(define (tap) (display "    "))
(define (emit4 line)
  (begin
    (tap)
    (display line)
    (newline)))

; tagged-list? : symbol value -> boolean
(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))
(define (define? exp)
  (tagged-list? 'define exp))
(define (if? exp)
  (tagged-list? 'if exp))
(define (void? exp)
  (eq? 'void (car exp)))
(define (lambda? exp)
  (tagged-list? 'lambda exp))
(define (prim? exp)
  (or (tagged-list? '+ exp)
      (tagged-list? '- exp)
      (tagged-list? '* exp)
      (tagged-list? '= exp)))
(define (<? exp)
  (tagged-list? '< exp))
(define (>? exp)
  (tagged-list? '> exp))
(define (=? exp)
  (tagged-list? '= exp))
(define (eq-exp? exp)
  (tagged-list? 'equal exp))
(define (gen exp)
  (display exp))

(define (func? exp)
  (pair? (cadr exp)))
(define (func-parm-length exp)
  (- (length exp) 1))
(define (get-ret exp)
  (cond
    ((symbol? exp) exp)
    ((number? exp) exp)))
(define (gen-template-line x y);default x 0 生成template<...>
  (if (= x 0)
      (void)
      (cond
        ((= y 0)
         (begin (gen "template<") (gen-template-line x 1)))
        ((= x y)
         (begin (gen "typename T") (printf "~a" y) (gen ">")) (newline))
        (else
         (begin (gen "typename T") (printf "~a" y) (gen ", ") (gen-template-line x (+ y 1)))))))
(define (gen-parm-val exp y);default x 1生成 T1 x;T2 y;（原函数的参数）
  (cond
    ((= (func-parm-length exp) y)
     (begin (tap) (printf "T~a ~a;" y (list-ref exp y)) (newline)))
    (else
     (begin (tap) (printf "T~a ~a;" y (list-ref exp y)) (newline) (gen-parm-val exp (+ 1 y))))))
(define (gen-constructor exp);生成构造函数
  (begin
    (display "    ")
    (printf "~a(" (car exp))
    (for ([i (func-parm-length exp)])
      (if (= i (- (func-parm-length exp) 1))
          (printf "T~a _t~a):" (+ i 1) (+ i 1))
          (printf "T~a _t~a, " (+ i 1) (+ i 1))))
    (for ([i (func-parm-length exp)])
      (if (= i (- (func-parm-length exp) 1))
          (printf "~a(_t~a){}" (list-ref exp (+ i 1)) (+ i 1))
          (printf "~a(_t~a), " (list-ref exp (+ i 1)) (+ i 1))))
    (newline)))

(define (gen-func-content exp);生成函数体
  (begin
    (emit4 "auto operator()()")
    (emit4 "{")
    (cond
      ((boolean? exp)
       (if (eq? exp #f)
           (begin (tap) (emit4 "return 0;"))
           (begin (tap) (emit4 "return 1;"))))
      ((number? exp)
       (begin (tap) (tap) (printf "return ~a" exp)))
      ((prim? exp)
       (begin (tap) (tap) (printf "return ~a ~a ~a;"
                                  (if (symbol? (list-ref exp 1))
                                      (list-ref exp 1)
                                      (deduce (list-ref exp 1)))
                                  (list-ref exp 0)
                                  (if (symbol? (list-ref exp 2))
                                      (list-ref exp 2)
                                      (deduce (list-ref exp 2))))))
      ((if? exp)
       (begin
         (cond
           ((<? (cadr exp))
            (begin (tap) (tap) (printf "if(~a ~a ~a)"
                                       (if (symbol? (list-ref (cadr exp) 1))
                                           (list-ref (cadr exp) 1)
                                           (deduce (list-ref (cadr exp) 1)))
                                       (list-ref (cadr exp) 0)
                                       (if (symbol? (list-ref (cadr exp) 2))
                                           (list-ref (cadr exp) 2)
                                           (deduce (list-ref (cadr exp) 2))))))
           ((>? (cadr exp))
            (begin (tap) (tap) (printf "if(~a ~a ~a)"
                                       (if (symbol? (list-ref (cadr exp) 1))
                                           (list-ref (cadr exp) 1)
                                           (deduce (list-ref (cadr exp) 1)))
                                       (list-ref (cadr exp) 0)
                                       (if (symbol? (list-ref (cadr exp) 2))
                                           (list-ref (cadr exp) 2)
                                           (deduce (list-ref (cadr exp) 2))))))
           ((=? (cadr exp))
            (begin (tap) (tap) (printf "if(~a ~a ~a)"
                                       (if (symbol? (list-ref (cadr exp) 1))
                                           (list-ref (cadr exp) 1)
                                           (deduce (list-ref (cadr exp) 1)))
                                       "=="
                                       (if (symbol? (list-ref (cadr exp) 2))
                                           (list-ref (cadr exp) 2)
                                           (deduce (list-ref (cadr exp) 2)))))))
         (newline)
         (tap) (emit4 "{") (newline)
         (tap) (tap) (tap) (printf "return ~a;" (get-ret (list-ref exp 2))) (newline)
         (tap) (emit4 "}")
         (tap) (tap) (emit "else")
         (tap) (emit4 "{") (newline)
         (tap) (tap) (tap) (printf "return ~a;" (get-ret (list-ref exp 3))) (newline)
         (tap) (emit4 "}"))))
    (emit4 "}")))
; define->class
(define (gen-func exp)
  (begin
    (gen-template-line (func-parm-length (car exp)) 0)
    (gen "class ")
    (printf "~a" (car (car exp)))(newline)
    (emit "{")
    (emit "public:")
    (gen-parm-val (car exp) 1)
    (gen-constructor (car exp))
    (gen-func-content (cadr exp))
    (emit "}")))
;  只支持两类：define和执行define过的过程
(define (trans-core exp)
  (cond
    ((define? exp)
     (if (func? exp)
          (gen-func (cdr exp))
          (gen-symbol (cdr exp))))
    (else
     (set! main-list (append main-list (gen-exec exp))))))

(define (rkt2cpp)
  (let ((prog (read)))
    (if (eq? prog eof)
        null;todo:display main-list
        (begin (trans-core prog) (rkt2cpp)))))
(rkt2cpp)