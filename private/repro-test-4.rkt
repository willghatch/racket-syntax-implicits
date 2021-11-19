#lang racket/base
(require
 "../main.rkt"
 (for-syntax racket/base
             syntax/parse))

(define-syntax-implicit i4 (syntax-parser [_ #'(printf "i4\n")]))

(provide i4)
