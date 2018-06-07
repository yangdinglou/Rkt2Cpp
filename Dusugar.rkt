#lang racket

; define? : term -> boolean
(define (define? sx)
  (match sx
    [`(define . ,_) #t]
    [else           #f]))

; not-define? : term -> boolean
(define (not-define? sx)
  (not (define? sx)))

; atomic? : term -> boolean
(define (atomic? exp)
  (match exp
    [`(λ . ,_)     #t]
    [(? number?)   #t]
    [(? string?)   #t]
    [(? boolean?)  #t]
    [`(quote . ,_) #t]
    ['(void)       #t]
    [else          #f]))

; atomic-define? : term -> boolean
(define (atomic-define? def)
  (match def
    [`(define ,v ,exp)  (atomic? exp)]
    [else               #f]))

; tops-to-defs : top list -> def list
(define (tops-to-defs tops)
  
  (define (top-to-def top)
    (match top
      [`(define (,f ,params ...) . ,body) 
       `(define ,f (λ ,params . ,body))]
    
      [`(define ,v ,exp)
       `(define ,v ,exp)]
    
      [exp
       `(define ,(gensym '_) ,exp)]))
  
  (map top-to-def tops))


      

;; Desugaring.

; desugar-quote : sexp -> exp
(define (desugar-quote s-exp)
  (cond
    [(pair? s-exp)     `(cons ,(desugar-quote (car s-exp))
                              ,(desugar-quote (cdr s-exp)))]
    [(null? s-exp)     ''()]
    [(number? s-exp)   s-exp]
    [(string? s-exp)   s-exp]
    [(boolean? s-exp)  s-exp]
    [(symbol? s-exp)   `(quote ,s-exp)]
    [else 
     (error (format "strange value in quote: ~s~n" s-exp))]))

; desugar-body : body -> exp
(define (desugar-body body)
  (match body
    [`(,exp)
     (desugar-exp exp)]
    
    [`(,(and (? not-define?) exps) ...)
     `(begin ,@(map desugar-exp exps))]
    
    [`(,tops ... ,exp)
     (define defs (tops-to-defs tops))
     (desugar-exp (match defs
                    [`((define ,vs ,es) ...)
                     `(letrec ,(map list vs es) ,exp)]))]))
       

; desugar-exp : exp -> exp
(define (desugar-exp exp)
  (match exp
    [(? symbol?)      exp]
    [`(quote ,s-exp)  (desugar-quote s-exp)]

    [`(let ((,vs ,es) ...) . ,body)
     `((λ ,vs ,(desugar-body body)) 
       ,@(map desugar-exp es))]
    
    [`(letrec ((,vs ,es) ...) . ,body)
     (desugar-exp
      `(let ,(for/list ([v vs])
               (list v '(void)))
         ,@(map (λ (v e)
                  `(set! ,v ,e))
                vs es)
         ,@body))]
    
    [`(λ ,params . ,body)
     `(λ ,params ,(desugar-body body))]
    
    [`(cond)
     '(void)]
    
    [`(cond (else ,exp))
     (desugar-exp exp)]
        
    [`(cond (,test ,exp))
     `(if ,(desugar-exp test) 
          ,(desugar-exp exp) 
          (void))]
    
    [`(cond (,test ,exp) ,rest ...)
     `(if ,(desugar-exp test)
          ,(desugar-exp exp)
          ,(desugar-exp `(cond . ,rest)))]
    
    [`(and)   #t]
    [`(or)    #f]
    
    [`(or ,exp)
     (desugar-exp exp)]
    
    [`(and ,exp)
     (desugar-exp exp)]
    
    [`(or ,exp . ,rest)
     (define $t (gensym 't))
     (desugar-exp 
      `(let ((,$t ,exp))
         (if ,$t ,$t (or . ,rest))))]
    
    [`(and ,exp . ,rest)
     `(if ,(desugar-exp exp)
          ,(desugar-exp `(and . ,rest))
          #f)]
    
    [`(if ,test ,exp)
     `(if ,(desugar-exp test) ,(desugar-exp exp) (void))]
    
    [`(if ,test ,exp1 ,exp2)
     `(if ,(desugar-exp test) 
          ,(desugar-exp exp1) 
          ,(desugar-exp exp2))]
    
    [`(set! ,v ,exp)
     `(set! ,v ,(desugar-exp exp))]
    
    [`(begin . ,body)
     (desugar-body body)]
    
    [(? atomic?)      exp]
    
    [`(,f . ,args)  
     `(,(desugar-exp f) ,@(map desugar-exp args))]
            
    [else 
     (printf "desugar fail: ~s~n" exp)
     exp]))

(define (pp x) (display (pretty-format x)))

(pp (desugar-exp 
     '(define (f x y)
        (let ((a 0))
          (if (and x y)
              (set! a x)
              (set! a y))
          a))))
          
      
