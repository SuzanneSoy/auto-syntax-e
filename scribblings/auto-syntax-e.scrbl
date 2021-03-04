#lang scribble/manual
@require[scribble/example
         @for-label[auto-syntax-e
                    racket/base
                    syntax/parse]]

@title{auto-syntax-e}
@author[@author+email["Suzanne Soy" "racket@suzanne.soy"]]

@defmodule[auto-syntax-e]

This package allows using syntax pattern variables outside of syntax
templates: when @racket[_x] is bound as a syntax pattern variable, writing
@racket[_x] then becomes roughly equivalent to
@racket[(syntax->datum #'_x-ddd)], where @racket[_x-ddd] is @racket[x] wrapped
under the appropriate number of ellipses. If the pattern variable is bound by
@racket[syntax-parse] and contains non-syntax parts (e.g. it was bound within
an @racket[~optional] clause, or using @racket[#:attr]), they are left
unchanged.

@defform[(auto-with-syntax ([patᵢ eᵢ] ...) body ...)]{
 Like @racket[(with-syntax ([patᵢ eᵢ] ...) body ...)], but the syntax pattern
 variables bound by the @racket[patᵢ ...] can be used outside of syntax patterns
 (they are implicitly transformed using @racket[syntax->datum]):

 @examples[#:eval ((make-eval-factory '(auto-syntax-e)))
           (auto-with-syntax ([x #'123])
             (list (add1 x) #'x))]}

@defform[(auto-syntax (pvarᵢ ...) body ...)]{
 Re-binds the syntax pattern variables @racket[pvarᵢ ...], so that can be used
 outside of syntax patterns like in @racket[auto-with-syntax]:

 @examples[#:eval ((make-eval-factory '(auto-syntax-e syntax/parse)))
           (syntax-parse #'(1 2 3)
             [(x:nat y:nat ...)
              (auto-syntax (x y)
                (list (map add1 (cons x y)) #'(x y ...)))])
           (syntax-parse #'(1 2 3)
             [({~seq x:nat {~optional y:nat}} ...)
              (auto-syntax (x y)
                (list (map cons x y)
                      (attribute x)
                      (attribute y)))])]

 When one of the @racket[pvarᵢ ...] is not a syntax pattern variable, it is
 ignored and the existing binding, if any, is left untouched.

 Note that it is not necessary to specify the ellipsis-depth of each
 @racket[pvarᵢ].}

@defform[(auto-syntax-case stx-expression (literal ...)
                           [patᵢ maybe-guardᵢ bodyᵢ]
                           ...)
         #:grammar
         [(maybe-guardᵢ (code:line)
                        (code:line guard-expression))]]{
 Like @racket[syntax-case], but the syntax pattern variables bound by the
 @racket[patᵢ ...] can be used outside of templates like in
 @racket[auto-with-syntax].}