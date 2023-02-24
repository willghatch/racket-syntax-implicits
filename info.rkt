#lang info
(define deps '("base"
               "rackunit-lib"
               ))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/syntax-implicits.scrbl" () (library))))
(define license '(Apache-2.0 OR MIT))

