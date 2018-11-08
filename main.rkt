#lang racket/base
(provide
 define-syntax-implicit
 with-syntax-implicits
 splicing-with-syntax-implicits
 (for-syntax
  syntax-implicit-value
  ))

(module syntax-implicit-struct racket/base
  (provide (all-defined-out))
  (define (sil->gensym-stx si-struct context-stx location-stx)
    (datum->syntax context-stx
                   (syntax-implicit-struct-gensym si-struct)
                   location-stx))
  (define (sil-val si-struct context-stx location-stx)
    (syntax-local-value (sil->gensym-stx si-struct context-stx location-stx)))
  (struct syntax-implicit-struct (gensym)
    #:property prop:procedure (λ (inst stx)
                                ((sil-val inst stx stx)
                                 stx))))

(require
 racket/splicing
 (for-syntax
  racket/base
  syntax/parse
  (submod "." syntax-implicit-struct)
  ))

(define-for-syntax (syntax-implicit-value stx #:context [context stx])
  (define slv (syntax-local-value stx))
  (if (syntax-implicit-struct? slv)
      (sil-val slv context stx)
      (error 'syntax-implicit-value "Not a syntax-implicit: ~a" stx)))

(define-syntax (define-syntax-implicit stx)
  (syntax-parse stx
    [(_ name:id binding:expr)
     (define implicit-gensym
       (gensym (format "~a_implicit-gensym_" (syntax->datum #'name))))
     (with-syntax ([implicit-gensym (datum->syntax #'name implicit-gensym)])
       #'(begin
           (define-syntax implicit-gensym binding)
           (define-syntax name (syntax-implicit-struct 'implicit-gensym))))]))


(define-for-syntax (with-syntax-implicit* stx context)
  (syntax-parse stx
    [(orig-macro let-form ([implicit-name new-val] ...) e ...+)
     (define context-use
       (or context
           (let ([contexts (map (λ (x) (datum->syntax x 'an-identifier))
                                (syntax->list #'(e ...)))])
             (unless (for/and ([x (cdr contexts)])
                       (bound-identifier=? (car contexts) x))
               (raise-syntax-error
                'with-syntax-implicit
                "Multiple body forms were given with different scoping information, so there is not a clear choice of info to bind the syntax-implicit to."
                #'orig-macro))
             (car contexts))))
     (with-syntax ([(implicit-gensym ...) (map (λ (x)
                                                 (datum->syntax
                                                  context-use
                                                  (syntax-implicit-struct-gensym
                                                   (syntax-local-value x))
                                                  x))
                                               (syntax->list #'(implicit-name ...)))])
       #'(let-form ([implicit-gensym new-val] ...)
                   e ...))]))

(begin-for-syntax
  (define-syntax-class syntax-implicit
    (pattern name:id
             #:when (syntax-implicit-struct?
                     (syntax-local-value #'name (λ () #f))))))

(define-syntax def-with-syntax-implicit
  (syntax-parser
    [(_ name:id let-form:id)
     #'(define-syntax name
         (syntax-parser
           [(wsi (~optional (~seq #:context context))
                 ([implicit:syntax-implicit new-val:expr] (... ...))
                 e ...+)
            (with-syntax-implicit*
              #'(wsi let-form ([implicit new-val] (... ...)) e (... ...))
              (attribute context))]))]))
(def-with-syntax-implicit with-syntax-implicits let-syntax)
(def-with-syntax-implicit splicing-with-syntax-implicits splicing-let-syntax)




(module+ test
  (require
   rackunit
   (for-syntax
    racket/base
    syntax/parse
    ))

  (define-syntax-implicit implicit-1 (syntax-parser [_ #''hello]))

  (check-equal?
   implicit-1
   'hello)

  (check-equal?
   (with-syntax-implicits ([implicit-1 (syntax-parser [_ #''goodbye])])
     implicit-1)
   'goodbye)

  (splicing-with-syntax-implicits
   ([implicit-1 (syntax-parser [_ #''jupiter])])
   (define-syntax (use-implicit/jupiter stx)
     (syntax-parse stx
       [(_) #'implicit-1])))

  (check-equal?
   (use-implicit/jupiter)
   'jupiter)

  ;; This is the real test.
  (check-equal?
   (with-syntax-implicits ([implicit-1 (syntax-parser [_ #''mars])])
     (use-implicit/jupiter))
   'jupiter)

  #|
  TODO - be sure I can make a macro-defining macro that uses the site where the new macro is defined as the site to get the implicit value from.
  TODO - test implicit chaining (IE an implicit uses and implicit which uses an implicit -- they should all get their implicit value from the same place).  For now this should use context on parentheses, I guess, but eventually there should be an implicit context syntax property.  The problem is that syntax properties don't accrue scopes, so going through macro transformations will break the context.
  TODO - create comparison examples -- make a normal binding, syntax parameter, and syntax implicit together, then show a bunch of similar uses of them to show how they differ.
  |#

  )
