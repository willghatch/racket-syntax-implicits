#lang racket/base
(require
 "../main.rkt"
 (for-syntax racket/base
             syntax/parse))

(define-syntax-implicit i3 (syntax-parser [_ #'(printf "i3\n")]))

(provide i3)
