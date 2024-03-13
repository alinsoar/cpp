#lang racket

(include "cc-include.rkt")

(define driver
  (lambda (filename)
    (__d "Compile" filename)
    (cpp filename
         (lambda (translation/unit)
           (for-each
            (lambda (a) (__d (a 'TREE)))
            translation/unit)
           (__d "FINE:" ERROR-COUNT)
           ))))

(let ((args (current-command-line-arguments)))
  (let ((filename (and (> (vector-length args) 0)
                       (vector-ref args 0))))
    (cond ((and (= 1 (vector-length args))
                (file-exists? filename))
           (driver filename))
          (else
           (__p "C Compiler\n"
                "Use it so: ``racket c-semantics.rkt FILE.C``\n")))))
