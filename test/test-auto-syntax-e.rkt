#lang racket

(require auto-syntax-e
         rackunit
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
                [2 #f]))

(check-equal? (match (auto-syntax-case #'123 ()
                       [x (list (add1 x) #'x)])
                [(list a (? syntax? b))
                 (list a (syntax-e b))]
                [_ 'error])
              '(124 123))