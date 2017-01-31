#lang racket
(require auto-syntax-e (for-syntax racket/base))
(auto-syntax-case #'(1 2 3) ()
  [(x ...)
   (map add1 x)])

(begin-for-syntax
  (require auto-syntax-e (for-syntax racket/base))
  (auto-syntax-case #'(1 2 3) ()
    [(x ...)
     (map add1 x)]))

(begin-for-syntax
  (begin-for-syntax
    (require auto-syntax-e (for-syntax racket/base))
    (auto-syntax-case #'(1 2 3) ()
      [(x ...)
       (map add1 x)])))

(begin-for-syntax
  (begin-for-syntax
    (begin-for-syntax
      (require auto-syntax-e (for-syntax racket/base))
      (auto-syntax-case #'(1 2 3) ()
        [(x ...)
         (map add1 x)]))))