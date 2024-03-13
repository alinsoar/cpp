;; -*- mode:scheme ; buffer-read-only:nil -*-

(define is/product/type? cons?)

(define is/sum/type? (lambda (obj) (not (is/product/type? obj))))

(define tree-repr
  (lambda (a)
    (if (procedure? a)
        (a 'TREE)
        (if (cons? a)
            (map tree-repr a)
            a))))

(define tree-type-check-list
  (lambda (this T? a)
    (map (lambda (x) (tree-type-check this T? x))
         a)))

(define tree-type-check-opt
  (lambda (this T? a)
    (tree-type-check this T? a)))

(define tree-type-check
  (lambda (this T? a)
    (or (T? a)
        (begin
          (PANIC (if (procedure? this)
                     this
                     this)
                 ":wrong type check"
                 "; applied --" a
                 "; needed -- ?" T?)
          (__d "expr:" a 'TREE "\n--")))))

(define LEX.string? string?)
(define LEX.key? symbol?)
(define LEX.id? symbol?)
(define LEX.punct? (lambda (a) 'ok))

(define ROOT.string? string?)
(define ROOT.integer? integer?)
(define ROOT.bottom? (lambda _ #t))
(define ROOT.boolean? (lambda (a) (or (eq? #t a) (eq? #f a))))
(define ROOT.set? set?)
(define ROOT.hash? hash?)
(define ROOT.symbol? symbol?)
(define ROOT.vector? vector?)
(define ROOT.box? box?)
(define ROOT.char? char?)
