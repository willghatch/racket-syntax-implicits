#lang racket/base
(require
 "../main.rkt"
 (for-syntax racket/base
             syntax/parse))

(define-syntax-implicit i1 (syntax-parser [_ #'(printf "i1\n")]))

(provide i1)
