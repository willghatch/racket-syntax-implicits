#lang racket/base
(require
 "../main.rkt"
 (for-syntax racket/base
             syntax/parse))

(define-syntax-implicit i2 (syntax-parser [_ #'(printf "i2\n")]))

(provide i2)
