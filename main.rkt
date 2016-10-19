#lang racket/base

(require racket/match
         syntax/parse
         (for-syntax racket/base
                     racket/syntax
                     racket/list
                     racket/struct
                     syntax/parse
                     racket/private/sc)
         ;; attribute-mapping? is provided for-syntax
         (only-in syntax/parse/private/residual attribute-mapping?))

(provide auto-with-syntax)
(provide auto-syntax)

(define (leaves->datum e depth)
  (if (> depth 0)
      (map (λ (eᵢ) (leaves->datum eᵢ (sub1 depth))) e)
      (if (syntax? e)
          (syntax->datum e)
          e)))
        

(define-syntax (to-datum stx)
  (syntax-case stx ()
    [(_ id)
     (syntax-pattern-variable? (syntax-local-value #'id (λ () #f)))
     (begin
       (let* ([mapping (syntax-local-value #'id)]
              [valvar (syntax-mapping-valvar mapping)]
              [depth (syntax-mapping-depth mapping)])
         (if (attribute-mapping? (syntax-local-value valvar (λ () #f)))
             #`(leaves->datum (attribute id) #,depth)
             #`(leaves->datum #,valvar #,depth))))]))

(begin-for-syntax
  (define-values (struct:auto-pvar
                  make-auto-pvar
                  auto-pvar?
                  auto-pvar-ref
                  auto-pvar-set!)
    (make-struct-type 'auto-pvar
                      (eval #'struct:syntax-mapping
                            (module->namespace 'racket/private/sc))
                      0
                      0
                      #f
                      null
                      (current-inspector)
                      (λ (self stx)
                        #`(to-datum #,stx)))))

(define-for-syntax (syntax->tree/ids e)
  (cond [(identifier? e) e]
        [(syntax? e) (syntax->tree/ids (syntax-e e))]
        [(pair? e) (cons (syntax->tree/ids (car e))
                         (syntax->tree/ids (cdr e)))]
        [(vector? e) (map syntax->tree/ids (vector->list e))]
        [(box? e) (syntax->tree/ids (unbox e))]
        [(prefab-struct-key e) (map syntax->tree/ids (struct->list e))]
        [else e]))

(define-for-syntax (syntax->ids e)
  (filter identifier? (flatten (syntax->tree/ids e))))

(define-syntax auto-syntax
  (syntax-parser
    [(_ (id ...) body ...)
     #:with (pvar-id ...) (filter (λ (id)
                                    (syntax-pattern-variable?
                                     (syntax-local-value id (λ () #f))))
                                  (syntax->list #'(id ...)))
     (with-disappeared-uses
      (let ()
        (record-disappeared-uses (syntax->list #'(pvar-id ...)))
        #'(let-syntax ([pvar-id
                        (make-set!-transformer
                         (let ([mapping (syntax-local-value
                                         (quote-syntax pvar-id))])
                           (make-auto-pvar (syntax-mapping-depth mapping)
                                           (syntax-mapping-valvar mapping))))]
                       ...)
            body ...)))]))

(define-syntax auto-with-syntax
  (syntax-parser
    [(_ ([pat e] ...) body ...)
     #:with (id ...) (syntax->ids #'(pat ...))
     #'(with-syntax ([pat e] ...)
         (auto-syntax (id ...)
           body ...))]))

(module+ test
  (require rackunit
           syntax/parse)
  (check-equal? (match (auto-with-syntax ([x #'123])
                         (list (add1 x) #'x))
                  [(list a (? syntax? b))
                   (list a (syntax-e b))]
                  [_ 'error])
                '(124 123))

  (check-equal? (match (syntax-parse #'(1 2 3)
                         [(x:nat y:nat ...)
                          (auto-syntax (x y)
                            (list (map add1 (cons x y)) #'(x y ...)))])
                  [(list a (? syntax? b))
                   (list a (syntax->datum b))]
                  [_ 'error])
                '((2 3 4) (1 2 3)))

  (check-equal? (match (syntax-parse #'(1 2 3)
                         [({~seq x:nat {~optional y:nat}} ...)
                          (auto-syntax (x y)
                            (list (map cons x y)
                                  (attribute x)
                                  (attribute y)))])
                  [(list a
                         (list (? syntax? b₁) (? syntax? b₂))
                         (list (? syntax? c₁) (and #f c₂)))
                   (list a
                         (list (syntax->datum b₁) (syntax->datum b₂))
                         (list (syntax->datum c₁) c₂))]
                  [_ 'error])
                '([(1 . 2) (3 . #f)]
                  [1 3]
                  [2 #f])))
