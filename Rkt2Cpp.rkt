#lang racket
(require "desugar2.rkt")
; tagged-list? : symbol value -> boolean
(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))

(define (void? exp)
  (eq? 'void (car exp)))
(define (lambda? exp)
  (or (tagged-list? 'lambda exp) (tagged-list? 'λ exp)))
(define (prim? exp)
  (or (tagged-list? '+ exp)
      (tagged-list? '- exp)
      (tagged-list? '* exp)
      (tagged-list? '/ exp)))
(define (define? exp)
  (tagged-list? 'define exp))
(define (begin? exp)
  (tagged-list? 'begin exp))
(define (if? exp)
  (tagged-list? 'if exp))
(define (set? exp)
  (tagged-list? 'set! exp))
(define (let? exp)
  (tagged-list? 'let exp))
(define (<? exp)
  (tagged-list? '< exp))
(define (>? exp)
  (tagged-list? '> exp))
(define (=? exp)
  (tagged-list? '= exp))
(define (eq-exp? exp)
  (tagged-list? 'equal exp))


(define main-list (list))
(define var-list (list))
(define func-list (list))
(define void-list (list 'void 'set!))
;作用：将racket内置函数用C++实现后
(define selfdefine (list 'even?
                         'odd?))

;构造函数参数
(define (gen-parm exp)
  (let ((ret ""))
    (if (= (length exp) 1)
        (string-replace ret ret (string-append ret (deduce (car exp)) ")"))
        (string-replace ret ret (string-append ret
                                               (deduce (car exp))
                                               ", "
                                               (gen-parm (cdr exp)))))))
;F tmp;[]{tmp();}();
(define (constructor-run exp)
  (list (string-append "[] {"
                       (symbol->string (car exp))
                       " tmp("
                       (gen-parm (cdr exp))
                       ";"
                       "return tmp();"
                       "}();")))
;f/(f a b)
(define (gen-exec exp)
      (deduce exp))
    
;todo

(define (void-func? exp)
  (if (pair? exp)
      (not (not (member (car exp) void-list)))
      #f))
(define (emit line)
  (display line)
  (newline))
(define (tap) (display "    "))
(define (emit4 line)
  (begin
    (tap)
    (display line)
    (newline)))

(define (gen-init-list exp)
  (let ((ret ""))
    (if (= (length exp) 1)
        (string-replace ret ret (string-append ret (deduce (car exp)) "}"))
        (string-replace ret ret (string-append ret
                                               (deduce (car exp))
                                               ", "
                                               (gen-init-list (cdr exp)))))))
(define (gen-begin-ret exp)
  (if (= (length exp) 1)
      (get-ret (car exp))
      (string-append (deduce (car exp)) (gen-begin-ret (cdr exp)))))
(define (func? exp)
  (pair? (cadr exp)))
(define (func-parm-length exp)
  (- (length exp) 1))
;所有define的结束点********************************************
(define (get-ret exp)
  (cond
    ((lambda? exp) (string-append "return [=]("
                                  (gen-lambda-parm (cadr exp))
                                  "{"
                                  (get-ret (caddr exp))
                                  "};"
                                  ))

    ((boolean? exp) (if exp "return true;" "return false;"))                        
    ((char? exp) (string-append "return \'" (~a exp) "\';"))
    ((symbol? exp) (string-append "return " (if (member exp var-list)
                                                (string-append (~a exp) "()")
                                                (~a exp)) ";"))
    ((number? exp) (string-append "return " (~a exp) ";"))
    ((string? exp) (string-append "return \"" (~a exp) "\";"))
    ((prim? exp)
     (string-append
      "return "
      (cond
        ((tagged-list? '+ exp) "sum(")
        ((tagged-list? '- exp) "sub(")
        ((tagged-list? '* exp) "mul(")
        ((tagged-list? '/ exp) "div("))
      (gen-parm (cdr exp))
      ";"))
    
    ((if? exp)
     (string-append
      (cond
        ((or (<? (cadr exp)) (>? (cadr exp)))
         (string-append "if("
                        (~a (if (or (symbol? (list-ref (cadr exp) 1)) (number? (list-ref (cadr exp) 1)))
                                (list-ref (cadr exp) 1)
                                (deduce (list-ref (cadr exp) 1))))
                        (~a (list-ref (cadr exp) 0))
                        (~a (if (or (symbol? (list-ref (cadr exp) 2)) (number? (list-ref (cadr exp) 2)))
                                (list-ref (cadr exp) 2)
                                (deduce (list-ref (cadr exp) 2))))
                        ")"))
        ((=? (cadr exp))
         (string-append "if("
                        (~a (if (or (symbol? (list-ref (cadr exp) 1)) (number? (list-ref (cadr exp) 1)))
                                (list-ref (cadr exp) 1)
                                (deduce (list-ref (cadr exp) 1))))
                        "=="
                        (~a (if (or (symbol? (list-ref (cadr exp) 2)) (number? (list-ref (cadr exp) 2)))
                                (list-ref (cadr exp) 2)
                                (deduce (list-ref (cadr exp) 2))))
                        ")"))
        (else
         (string-append "if(" (~a (deduce (cadr exp))) ")" )))
      "\n{\n"
      (get-ret (list-ref exp 2)) "\n}\n"
      "else\n{\n"
      (get-ret (list-ref exp 3)) "\n}\n"))
    ((set? exp)
     (string-append (~a (deduce (list-ref exp 1))) "=" (~a (deduce (list-ref exp 2))) ";\nreturn;"))
    ((begin? exp)
       (gen-begin-ret (cdr exp)))
    ((void? exp) "return;")
    ((pair? exp)
     (string-append
      "return "
      (deduce exp)
      ";"))
    (else
     (error "ret ERROR"))
    ))

(define (gen-lambda-parm exp)
  (let ((ret ""))
    (if (= (length exp) 1)
        (string-replace ret ret (string-append ret "auto " (symbol->string (car exp)) ")"))
        (string-replace ret ret (string-append ret
                                               "auto "
                                               (symbol->string (car exp))
                                               ", "
                                               (gen-lambda-parm (cdr exp)))))))
;生成表达式
(define (deduce exp)
  (cond
    ((if? exp)
     (if (void-func? (list-ref exp 2))
         (string-append
          (cond
            ((or (<? (cadr exp)) (>? (cadr exp)))
             (string-append "if("
                            (~a (if (or (symbol? (list-ref (cadr exp) 1)) (number? (list-ref (cadr exp) 1)))
                                    (list-ref (cadr exp) 1)
                                    (deduce (list-ref (cadr exp) 1))))
                            (~a (list-ref (cadr exp) 0))
                            (~a (if (or (symbol? (list-ref (cadr exp) 2)) (number? (list-ref (cadr exp) 2)))
                                    (list-ref (cadr exp) 2)
                                    (deduce (list-ref (cadr exp) 2))))
                            ")"))
            ((=? (cadr exp))
             (string-append "if("
                            (~a (if (or (symbol? (list-ref (cadr exp) 1)) (number? (list-ref (cadr exp) 1)))
                                    (list-ref (cadr exp) 1)
                                    (deduce (list-ref (cadr exp) 1))))
                            "=="
                            (~a (if (or (symbol? (list-ref (cadr exp) 2)) (number? (list-ref (cadr exp) 2)))
                                    (list-ref (cadr exp) 2)
                                    (deduce (list-ref (cadr exp) 2))))
                            ")"))
            (else
             (string-append "if(" (~a (deduce (cadr exp))) ")" )))
          "\n{"
          (deduce (list-ref exp 2)) "\n}\n{"
          (deduce (list-ref exp 3)) "\n}\n")
         (string-append
          (cond
            ((or (<? (cadr exp)) (>? (cadr exp)))
             (string-append "("
                            (~a (if (or (symbol? (list-ref (cadr exp) 1)) (number? (list-ref (cadr exp) 1)))
                                    (list-ref (cadr exp) 1)
                                    (deduce (list-ref (cadr exp) 1))))
                            (~a (list-ref (cadr exp) 0))
                            (~a (if (or (symbol? (list-ref (cadr exp) 2)) (number? (list-ref (cadr exp) 2)))
                                    (list-ref (cadr exp) 2)
                                    (deduce (list-ref (cadr exp) 2))))
                            ")?"))
            ((=? (cadr exp))
             (string-append "("
                            (~a (if (or (symbol? (list-ref (cadr exp) 1)) (number? (list-ref (cadr exp) 1)))
                                    (list-ref (cadr exp) 1)
                                    (deduce (list-ref (cadr exp) 1))))
                            "=="
                            (~a (if (or (symbol? (list-ref (cadr exp) 2)) (number? (list-ref (cadr exp) 2)))
                                    (list-ref (cadr exp) 2)
                                    (deduce (list-ref (cadr exp) 2))))
                            ")?"))
            (else
             (string-append "(" (~a (deduce (cadr exp))) ")?" )))
          "\n("
          (deduce (list-ref exp 2)) ")\n:("
          (deduce (list-ref exp 3)) ")\n")
         ))
    ((lambda? exp) (string-append "[=]("
                                  (gen-lambda-parm (cadr exp))
                                  "{"
                                  (get-ret (caddr exp))
                                  "}"
                                  ))
    ((prim? exp)
     (string-append
      (cond
        ((tagged-list? '+ exp) "sum(")
        ((tagged-list? '- exp) "sub(")
        ((tagged-list? '* exp) "mul(")
        ((tagged-list? '/ exp) "divi("))
      (gen-parm (cdr exp))))
    
    ((set? exp)
     (string-append (~a (deduce (list-ref exp 1))) "=" (~a (deduce (list-ref exp 2))) ";"))
    ((pair? exp)
     (if (member (car exp) selfdefine)
         (string-append
          (cond
            ((tagged-list? 'even? exp) "even(")
            ((tagged-list? 'odd? exp) "odd("))
          (gen-parm (cdr exp)))
         (if (member (car exp) func-list)
             (string-append (deduce (car exp))
                            "("
                            (gen-parm (cdr exp))
                            "()")
             (string-append (deduce (car exp))
                            "("
                            (gen-parm (cdr exp))))))
    ((boolean? exp) (if exp "true" "false"))
    ((char? exp) (~a exp))
    ((symbol? exp)
     (if (member exp var-list)
         (string-append (~a exp) "()")
         (~a exp)))
    ((number? exp) (~a exp))
    ((string? exp) (~a exp))
    ))

(define (gen-template-line x y);default x 0 生成template<...>
  (if (= x 0)
      (void)
      (cond
        ((= y 0)
         (begin (display "template<") (gen-template-line x 1)))
        ((= x y)
         (begin (display "typename T") (printf "~a" y) (display ">")) (newline))
        (else
         (begin (display "typename T") (printf "~a" y) (display ", ") (gen-template-line x (+ y 1)))))))
(define (gen-parm-val exp y);default x 1生成 T1 x;T2 y;(原函数的参数)
  (cond
    ((= (func-parm-length exp) 0)
     (void))
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
    (if (= (func-parm-length exp) 0)
        (emit "){}")
        (void))
    (for ([i (func-parm-length exp)])
      (if (= i (- (func-parm-length exp) 1))
          (printf "~a(_t~a){}" (list-ref exp (+ i 1)) (+ i 1))
          (printf "~a(_t~a), " (list-ref exp (+ i 1)) (+ i 1))))
    (newline)))

(define (gen-func-content exp);生成函数体
  (begin
    (emit4 "auto operator()()")
    (emit4 "{")
    (emit (get-ret exp))
    (emit4 "}")))
; define->class
(define (gen-func exp)
  (begin
    (gen-template-line (func-parm-length (car exp)) 0)
    (display "class ")
    (printf "~a" (car (car exp)))(newline)
    (emit "{")
    (emit "public:")
    (gen-parm-val (car exp) 1)
    (gen-constructor (car exp))
    (gen-func-content (cadr exp))
    (emit "};")))

(define (gen-symbol exp)
  (begin
    (display "class ")
    (printf "~a" (car exp))(newline)
    (emit "{")
    (emit "public:")
    
    (gen-func-content (cadr exp))
    (printf "}~a;" (car exp))
    (newline)))

(define (put-main)
  (begin
    (emit "int main()")
    (emit "{")
    (for/list ([i main-list])
      (printf "if (!is_void<decltype(~a)>::value) cout << ~a << '\\n';\n" i i))
    (emit "return 0;")
    (emit "}")))
;  只支持两类：define和执行define过的过程
(define (trans-core exp)
  (cond
    ((define? exp)
     (if (func? exp)
         (begin (set! func-list (append func-list (list (car (cadr exp))))) (gen-func (cdr exp)))
         (begin (set! var-list (append var-list (list (cadr exp))))(gen-symbol (cdr exp)))))
    (else
     (set! main-list (append main-list (list (gen-exec exp)))))))


(define (rkt2cpp)
  (let ((prog (read)))
    (if (eq? prog eof)
        (put-main);todo:display main-list
        (begin (trans-core (desugar-exp prog)) (rkt2cpp)))))
(emit "#include \"rkt2cpp.h\"")
(rkt2cpp)
