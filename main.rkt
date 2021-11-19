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
  (define (sil->anchor-stx si-struct context-stx location-stx)
    (datum->syntax context-stx
                   (syntax-implicit-struct-anchor si-struct)
                   location-stx))
  (define (sil-val si-struct context-stx location-stx)
    (syntax-local-value (sil->anchor-stx si-struct context-stx location-stx)
                        (λ () (syntax-implicit-struct-original-default si-struct))))

  (define (syntax-implicit-value stx #:context [context stx])
    (define slv (syntax-local-value stx))
    (if (syntax-implicit-struct? slv)
        (sil-val slv context stx)
        (error 'syntax-implicit-value "Not a syntax-implicit: ~a" stx)))

  (struct syntax-implicit-struct (anchor original-default)
    #:property
    prop:procedure
    (λ (inst stx)
      ((sil-val inst stx stx) stx))))

(require
 racket/splicing
 (for-syntax
  racket/base
  syntax/parse
  (submod "." syntax-implicit-struct)
  ))

(define-syntax (define-syntax-implicit stx)
  (syntax-parse stx
    [(_ name:id binding:expr)
     (define implicit-anchor
       (string->uninterned-symbol
        (format "~a_implicit-anchor_" (syntax->datum #'name))))
     (with-syntax ([implicit-anchor (datum->syntax #'name implicit-anchor)])
       #'(begin
           (define-for-syntax implicit-original-binding binding)
           (define-syntax implicit-anchor implicit-original-binding)
           (define-syntax name (syntax-implicit-struct
                                'implicit-anchor
                                implicit-original-binding))))]))


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
     (with-syntax ([(implicit-anchor ...) (map (λ (x)
                                                 (datum->syntax
                                                  context-use
                                                  (syntax-implicit-struct-anchor
                                                   (syntax-local-value x))
                                                  x))
                                               (syntax->list #'(implicit-name ...)))])
       #'(let-form ([implicit-anchor new-val] ...)
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
   racket/stxparam
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
     (list
      (use-implicit/jupiter)
      (implicit-1)))
   '(jupiter mars))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Test chaining

  (define-syntax-implicit chain-1
    (syntax-parser
      [stx #`(list 'chain-1-a #,(datum->syntax #'stx (list #'chain-2)))]))
  (define-syntax-implicit chain-2
    (syntax-parser
      [stx #`(list 'chain-2-a #,(datum->syntax #'stx (list #'chain-3)))]))
  (define-syntax-implicit chain-3
    (syntax-parser
      [stx #`(list 'chain-3-a)]))

  (check-equal?
   (chain-1)
   '(chain-1-a (chain-2-a (chain-3-a))))

  (with-syntax-implicits ([chain-2
                           (syntax-parser
                             [stx #`(list 'chain-2-b
                                          #,(datum->syntax #'stx (list #'chain-3)))])])
    (check-equal?
     (chain-1)
     '(chain-1-a (chain-2-b (chain-3-a))))

    (with-syntax-implicits ([chain-1
                             (syntax-parser
                               [stx #`(list 'chain-1-b
                                            #,(datum->syntax #'stx
                                                             (list #'chain-2)))])]
                            [chain-3 (syntax-parser [stx #`(list 'chain-3-b)])])
      (check-equal? (chain-1)
                    '(chain-1-b (chain-2-b (chain-3-b))))))

  (with-syntax-implicits ([chain-1
                           (syntax-parser
                             [stx #`(list 'chain-1-c
                                          #,(datum->syntax #'stx
                                                           (list #'chain-2)))])])
    (check-equal? (chain-1)
                  '(chain-1-c (chain-2-a (chain-3-a)))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Test macro-defining macros


  (splicing-with-syntax-implicits
   ([implicit-1 (syntax-parser [_ #''in-macro-definitions-1])])
   ;; This one defines a macro that uses the context that the NEW macro is
   ;; defined in -- IE it uses some future context but not necessarily the
   ;; final context like a syntax parameter would.  It ignores the context HERE.
   (define-syntax (define-macro-that-uses-context stx)
     (syntax-parse stx
       [(_ name)
        #'(define-syntax (name i-stx)
            (syntax-parse i-stx
              [_ (datum->syntax #'name (list #'implicit-1))]))]))
   ;; This one ignores all later context -- it uses the context HERE.
   (define-syntax (define-macro-that-ignores-context stx)
     (syntax-parse stx
       [(_ name)
        #'(define-syntax (name i-stx)
            (syntax-parse i-stx
              [_ #'(implicit-1)]))])))

  (splicing-with-syntax-implicits
   ([implicit-1 (syntax-parser [_ #''in-definition-1])])
   (define-macro-that-uses-context use-1)
   (define-macro-that-ignores-context ignore-1))

  (with-syntax-implicits ([implicit-1
                           (syntax-parser [_ #''in-within-macro-use-use-1])])
    (check-equal?
     (implicit-1)
     'in-within-macro-use-use-1)
    (check-equal?
     (use-1)
     'in-definition-1)
    (check-equal?
     (ignore-1)
     'in-macro-definitions-1))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Compare let-syntax, syntax-parameterize, with-syntax-implicits
  (define-syntax c-ls1 (syntax-parser [_ #''c-ls1]))
  (define-syntax-parameter c-sp1 (syntax-parser [_ #''c-sp1]))
  (define-syntax-implicit c-si1 (syntax-parser [_ #''c-si1]))

  (splicing-let-syntax ([c-ls1 (syntax-parser [_ #''c-ls1_splice-1])])
    (splicing-syntax-parameterize ([c-sp1 (syntax-parser [_ #''c-sp1_splice-1])])
      (splicing-with-syntax-implicits
       ([c-si1 (syntax-parser [_ #''c-si1_splice-1])])

       (define-syntax (define-si-macro-that-uses-context stx)
         (syntax-parse stx
           [(_ name)
            #'(define-syntax (name i-stx)
                (syntax-parse i-stx
                  [_ (datum->syntax #'name (list #'c-si1))]))]))
       (define-syntax (define-si-macro-that-ignores-context stx)
         (syntax-parse stx
           [(_ name)
            #'(define-syntax (name i-stx)
                (syntax-parse i-stx
                  [_ #'(c-si1)]))]))
       (define-syntax (define-sp-macro-that-wants-to-use-context stx)
         (syntax-parse stx
           [(_ name)
            #'(define-syntax (name i-stx)
                (syntax-parse i-stx
                  [_ (datum->syntax #'name (list #'c-sp1))]))]))
       (define-syntax (define-sp-macro-that-ignores-context stx)
         (syntax-parse stx
           [(_ name)
            #'(define-syntax (name i-stx)
                (syntax-parse i-stx
                  [_ #'(c-sp1)]))]))
       (define-syntax (define-ls-macro-that-wants-to-use-context stx)
         (syntax-parse stx
           [(_ name)
            #'(define-syntax (name i-stx)
                (syntax-parse i-stx
                  [_ (datum->syntax #'name (list #'c-ls1))]))]))
       (define-syntax (define-ls-macro-that-ignores-context stx)
         (syntax-parse stx
           [(_ name)
            #'(define-syntax (name i-stx)
                (syntax-parse i-stx
                  [_ #'(c-ls1)]))]))
       (define-syntax (define-ls-macro-that-uses-context/hygiene-bending stx)
         (syntax-parse stx
           [(_ name)
            #'(define-syntax (name i-stx)
                (syntax-parse i-stx
                  [_ (datum->syntax #'name (list 'c-ls1))]))]))
       (define-syntax (define-ls-macro-that-uses-context/hygiene-bending-to-final-use stx)
         (syntax-parse stx
           [(_ name)
            #'(define-syntax (name i-stx)
                (syntax-parse i-stx
                  [_ (datum->syntax i-stx (list 'c-ls1))]))]))
       )))

  (splicing-let-syntax ([c-ls1 (syntax-parser [_ #''c-ls1_splice-2])])
    (splicing-syntax-parameterize ([c-sp1 (syntax-parser [_ #''c-sp1_splice-2])])
      (splicing-with-syntax-implicits
       ([c-si1 (syntax-parser [_ #''c-si1_splice-2])])
       (define-si-macro-that-uses-context c-si1_use)
       (define-si-macro-that-ignores-context c-si1_ignore)
       (define-sp-macro-that-wants-to-use-context c-sp1_use)
       (define-sp-macro-that-ignores-context c-sp1_ignore)
       (define-ls-macro-that-wants-to-use-context c-ls1_use)
       (define-ls-macro-that-ignores-context c-ls1_ignore)
       (define-ls-macro-that-uses-context/hygiene-bending c-ls1_bend)
       (define-ls-macro-that-uses-context/hygiene-bending-to-final-use c-ls1_bend-final)
       )))


  (splicing-let-syntax ([c-ls1 (syntax-parser [_ #''c-ls1_splice-3])])
    (splicing-syntax-parameterize ([c-sp1 (syntax-parser [_ #''c-sp1_splice-3])])
      (splicing-with-syntax-implicits
       ([c-si1 (syntax-parser [_ #''c-si1_splice-3])])
       #|
       "use" here means that the defining macro bent the parenthesis but not the id.
       Bending the id for syntax parameters and syntax implicits would mean likely
       no longer being bound to the parameter/implicit anymore.
       |#
       ;;;; syntax parameters
       (check-equal? c-sp1_ignore 'c-sp1_splice-3)
       (check-equal? c-sp1_use 'c-sp1_splice-3)
       (check-equal? c-sp1 'c-sp1_splice-3)
       ;;;; syntax implicits
       (check-equal? c-si1_ignore 'c-si1_splice-1)
       (check-equal? c-si1_use 'c-si1_splice-2)
       (check-equal? c-si1 'c-si1_splice-3)
       ;;;; let-syntax (normal identifiers)
       (check-equal? c-ls1_ignore 'c-ls1_splice-1)
       (check-equal? c-ls1_use 'c-ls1_splice-1)
       (check-equal? c-ls1 'c-ls1_splice-3)
       (check-equal? c-ls1_bend 'c-ls1_splice-2)
       (check-equal? c-ls1_bend-final 'c-ls1_splice-3)
       )))

  (let ()
    ;; And now a context where things have not been let/parameterize/with-ed
    ;;;; syntax parameters
    (check-equal? c-sp1_ignore 'c-sp1)
    (check-equal? c-sp1_use 'c-sp1)
    (check-equal? c-sp1 'c-sp1)
    ;;;; syntax implicits
    (check-equal? c-si1_ignore 'c-si1_splice-1)
    (check-equal? c-si1_use 'c-si1_splice-2)
    (check-equal? c-si1 'c-si1)
    ;;;; let-syntax (normal identifiers)
    (check-equal? c-ls1_ignore 'c-ls1_splice-1)
    (check-equal? c-ls1_use 'c-ls1_splice-1)
    (check-equal? c-ls1 'c-ls1)
    (check-equal? c-ls1_bend 'c-ls1_splice-2)
    (check-equal? c-ls1_bend-final 'c-ls1)
    )

  (let-syntax ([c-sp1 (syntax-parser [_ #''not-a-syntax-parameter])]
               [c-si1 (syntax-parser [_ #''not-a-syntax-implicit])])
    ;; Syntax parameters and syntax implicits get to do their “bending” even
    ;; if their names are shadowed.
    (check-equal? c-sp1_ignore 'c-sp1)
    (check-equal? c-sp1_use 'c-sp1)
    (check-equal? c-si1_ignore 'c-si1_splice-1)
    (check-equal? c-si1_use 'c-si1_splice-2)
    )

  #|
  My “syntax implicits” are like syntax parameters in that they have a hygienic identifier while in another sense bending hygiene.
  However, they can bend hygiene in a different way, more like normal identifiers.
  They don't solve quite the same problem as syntax parameters (they are complimentary), but they have the hygienic identifier in the same way.
  Are there other things in this space?  Syntax parameters get their value from the context where they end up, while syntax implicits get their value from some place where it was anchored (eg. by a source use OR a macro definition site).  Is there another place it would make sense to get a value from?

  Also, what is a better name?  “Syntax Implicits” are so-named because I want to use them as *implicit* identifiers that may not even be bound in the context where they are used (IE they need syntax-parameter-style lookup that uses a hygienic name but a different context from the binding), but that get their binding from some anchor (more like a normal identifier that can be let-syntax-ed at a usage or at a macro definition site).  But their behavior in general has nothing to do with the implicit-ness of my use-case.
  |#

  )
