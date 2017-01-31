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
(provide auto-syntax-case)
(module+ utils
  (provide (for-syntax make-auto-pvar
                       auto-pvar?)))

(define (leaves->datum e depth)
  (if (eq? e #f) ;; for attributes with ~optional holes.
      e
      (if (> depth 0)
          (map (λ (eᵢ) (leaves->datum eᵢ (sub1 depth))) e)
          (if (syntax? e)
              (syntax->datum e)
              e))))
        

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
  (define (auto-pvar-proc self stx)
    (cond
      [(identifier? stx)
       (datum->syntax stx
                      `(,(quote-syntax to-datum) ,stx)
                      stx
                      stx)]
      [(and (pair? (syntax-e stx))
            (identifier? (car (syntax-e stx))))
       (datum->syntax stx
                      `((,(quote-syntax to-datum) ,(car (syntax-e stx)))
                        .
                        ,(cdr (syntax-e stx)))
                      stx
                      stx)]
      [else (raise-syntax-error
             'auto-syntax-e
             "Improper use of auto-syntax-e pattern variable"
             stx)]))
  (define-values (struct:auto-pvar
                  -make-auto-pvar
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
                      auto-pvar-proc))
  (define (make-auto-pvar depth valvar)
    (make-set!-transformer (-make-auto-pvar depth valvar))))

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
                        (let ([mapping (syntax-local-value
                                        (quote-syntax pvar-id))])
                          (make-auto-pvar (syntax-mapping-depth mapping)
                                          (syntax-mapping-valvar mapping)))]
                       ...)
            body ...)))]))

(define-syntax auto-with-syntax
  (syntax-parser
    [(_ ([pat e] ...) body ...)
     #:with (id ...) (syntax->ids #'(pat ...))
     #'(with-syntax ([pat e] ...)
         (auto-syntax (id ...)
           body ...))]))


(define-syntax auto-syntax-case
  (syntax-parser
    [(_ stx-expression literals [pat guard+body ...] ...)
     #:with (id ...) (syntax->ids #'(pat ...))
     #'(syntax-case stx-expression literals
         [pat (auto-syntax (id ...)
                guard+body ...)]
         ...)]))