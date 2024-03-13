;; -*- mode:scheme ; buffer-read-only:t -*-

;;; Interface for trees written in lisp format TREE.DEF

;;; This is imported by ASDL-GEN.  ASDL-GEN is specific to each tree.

;;;                                          ASDL SELECTORS
;;; ============================================================

(define TREE:GET-MODULE-NAME
  cadar)
(define TREE:GET-PROPERTIES
  cddar)
(define TREE:GET-IMPORTED-MODULES
  (lambda (tree)
    (filter (lambda (a) (eq? 'IMPORT (car a)))
            (TREE:GET-PROPERTIES tree))))
(define TREE:GET-TYPE-LIST
  cdr)

(define IMPORT-LIST:GET-MODULE
  cadr)
(define IMPORT-LIST:GET-TYPES
  cddr)

(define TYPE:GET-KIND
  car)
(define TYPE:GET-NAME
  cadr)
(define TYPE:GET-POLYMORPHIC-ATTRIBUTES
  (lambda (x)
    (car (cdaddr x))))
(define TYPESUM:GET-CONSTRUCTORS
  (lambda (T)
    (and (eq? 'TYPE-SUM (TYPE:GET-KIND T))
         (filter (lambda (a)
                   (let ((CH (string-ref (symbol->string (car a)) 0)))
                     "[A-Z_]"
                     (or (char-upper-case? CH)
                         (char=? CH #\_))))
                 (cddr T)))))
(define TYPEPROD:GET-PARAMETERS
  (lambda (T)
    (and (eq? 'TYPE-PRODUCT (TYPE:GET-KIND T))
         (caddr T))))
(define TYPE:GET-ATTRIBUTES
  (lambda (T)
    (and (eq? 'TYPE-SUM (TYPE:GET-KIND T))
         (let ((a (filter (lambda (a)
                            (eq? (car a) 'attributes))
                          (cddr T))))
           (and (cons? a) (cdar a))))))

(define CONS:GET-ID
  car)
(define CONS:GET-PARAMETERS
  cdr)
(define CONS:GET-PARAMETER-COUNT
  (lambda (c)
    (PARAMETER-LIST:COUNT (CONS:GET-PARAMETERS c))))
(define CONS:GET-PARAMETER-TYPES
  (lambda (c)
    (PARAMETER-LIST:TYPES (CONS:GET-PARAMETERS c))))
(define CONS:GET-PARAMETER-TYPE-OPT
  (lambda (c)
    (PARAMETER-LIST:TYPE-OPT (CONS:GET-PARAMETERS c))))
(define CONS:GET-PARAMETER-IDS
  (lambda (c)
    (PARAMETER-LIST:IDS (CONS:GET-PARAMETERS c))))

(define PARAMETER-LIST:COUNT
  (lambda (p*)
    (length p*)))
(define PARAMETER-LIST:TYPES
  (lambda (p*)
    (map car p*)))
(define PARAMETER-LIST:TYPE-OPT
  (lambda (p*)
    (map cadr p*)))
(define PARAMETER-LIST:IDS
  (lambda (p*)
    (map caddr p*)))

;;;        CONVERT THE ASDL TREE IN INTERNAL SCHEME DATATYPES
;;; ============================================================

(define ~TREE/MODULE 'nil)

(define ~TYPE-NAME-LIST 'nil)
(define ~TYPESUM->CONSTRUCTOR-ID-LIST (make-hash))
(define ~TYPESUM->POLYMORPHIC-ATTRIBUTES-LIST (make-hash))
(define ~TYPEPROD->PARAMETER-LIST (make-hash))
(define ~TYPE->KIND (make-hash))
(define ~TYPE->ATTRIBUTES (make-hash))
(define ~TYPE->MODULE (make-hash))

(define ~CONSTRUCTOR->PARAM-TYPES (make-hash))
(define ~CONSTRUCTOR->PARAM-IDS (make-hash))
(define ~CONSTRUCTOR->PARAM-COUNT (make-hash))
(define ~CONSTRUCTOR->TYPE (make-hash))

;;;                                                   SETTER
;;; ============================================================

(define set-module!
  (lambda (tree)
    (set! ~TREE/MODULE (TREE:GET-MODULE-NAME tree))))

(define set-imported-types!
  (lambda (tree)
    (for-each 
     (lambda (il)
       (define MOD (IMPORT-LIST:GET-MODULE il))
       (for-each (lambda (ty)
                   (hash-set! ~TYPE->MODULE ty MOD))
                 (IMPORT-LIST:GET-TYPES il)))
     (TREE:GET-IMPORTED-MODULES tree))))

(define set-types!
  (lambda (tree)
    
    (define +ty->mod
      (lambda (t/name)
        (hash-set! ~TYPE->MODULE t/name ~TREE/MODULE)))
    
    (define +sumty->ko*
      (lambda (t/name c/names)
        (hash-set! ~TYPESUM->CONSTRUCTOR-ID-LIST t/name c/names)))

    (define +prodty->param*
      (lambda (t/name c/names)
        (hash-set! ~TYPEPROD->PARAMETER-LIST t/name c/names)))
    
    (define +ko*->sumty
      (lambda (t/name c/names)
        (map
         (lambda (ko)
           (hash-set! ~CONSTRUCTOR->TYPE ko t/name))
         c/names)))
    
    (define +ko->param
      (lambda (c/id c/p/ty c/p/ids c/p/count)
        (hash-set! ~CONSTRUCTOR->PARAM-TYPES c/id c/p/ty)
        (hash-set! ~CONSTRUCTOR->PARAM-IDS c/id c/p/ids)
        (hash-set! ~CONSTRUCTOR->PARAM-COUNT c/id c/p/count)))
    
    (define +ty->kind
      (lambda (t/name t/kind)
        (hash-set! ~TYPE->KIND t/name t/kind)))
    
    (define +ty->attr
      (lambda (t/name t/attr)
        (hash-set! ~TYPE->ATTRIBUTES t/name t/attr)))
    
    (define +ty->polyattr
      (lambda (t/name t/attr)
        (hash-set! ~TYPESUM->POLYMORPHIC-ATTRIBUTES-LIST t/name t/attr)))
    
    (define iter/type/cons
      (lambda (c* col)
        (if (null? c*)
            (col '())
            (let ((C0 (car c*)))
              (let ((c/id (CONS:GET-ID C0))
                    (c/p/count (CONS:GET-PARAMETER-COUNT C0))
                    (c/p/ty (map cons
                                 (CONS:GET-PARAMETER-TYPES C0)
                                 (CONS:GET-PARAMETER-TYPE-OPT C0)))
                    (c/p/ids (CONS:GET-PARAMETER-IDS C0)))
                ;; (__d "......" c/id)
                (+ko->param c/id c/p/ty c/p/ids c/p/count)
                (iter/type/cons
                 (cdr c*)
                 (lambda (c/names)
                   (col (cons c/id c/names)))))))))
    
    (define iter/types
      (lambda (T* col)
        (if (null? T*)
            (col '())
            (let ((T0 (car T*)))
              (let ((t/kind(TYPE:GET-KIND T0))
                    (t/name(TYPE:GET-NAME T0))
                    (t/polyattr(TYPE:GET-POLYMORPHIC-ATTRIBUTES T0))
                    (t/attr(TYPE:GET-ATTRIBUTES T0)))
                ;; (__d ".." t/name)
                (+ty->polyattr t/name t/polyattr)
                (+ty->kind t/name t/kind)
                (+ty->attr t/name t/attr)
                (+ty->mod t/name)
                (iter/types
                 (cdr T*)
                 (lambda (all/type/names)
                   (if (eq? 'TYPE-SUM t/kind)
                       (let ((t/cons(TYPESUM:GET-CONSTRUCTORS T0)))
                         (iter/type/cons
                          t/cons
                          (lambda (c/names)
                            ;; (__d "==" t/name ";" c/names)
                            (+sumty->ko* t/name c/names)
                            (+ko*->sumty t/name c/names)
                            (col (cons t/name all/type/names)))))
                       (if (eq? 'TYPE-PRODUCT t/kind)
                           (let ((t/params(TYPEPROD:GET-PARAMETERS T0)))
                             (+prodty->param* t/name t/params)
                             (col (cons t/name
                                        all/type/names)))
                           (error "set-types! iter"))))))))))
    
    (iter/types (TREE:GET-TYPE-LIST tree)
                (lambda (type-name-list)
                  (set! ~TYPE-NAME-LIST type-name-list)
                  '(__d "?" )))))

;;;             SELECTORS / PUBLIC INTERFACE (for asdlgen)
;;; ============================================================

;;;       *** DO NOT LOOP OVER THE ASDL TREES FROM .DEF FILES.  ***

;;;       Use this interface only to loop over.  Start by calling
;;;       set-asdl-tree!

(define asdl-def-load
  (lambda (file)
    (set-asdl-tree! (file->list file))
    'DONE))

(define set-asdl-tree!
  (lambda (tree)
    (set-module! tree)
    (set-imported-types! tree)
    (set-types! tree)))

(define %GET-MODULE
  (lambda ()
    ~TREE/MODULE))

(define %GET-TYPE-NAME-LIST
  (lambda ()
    ~TYPE-NAME-LIST))
(define %GET-TYPE->MODULE
  (lambda (T)
    (hash-ref ~TYPE->MODULE T)))
(define %GET-TYPE->KIND
  (lambda (T)
    (hash-ref ~TYPE->KIND T #f)))
(define %GET-TYPESUM->CONSTRUCTOR-ID-LIST
  (lambda (T)
    (hash-ref ~TYPESUM->CONSTRUCTOR-ID-LIST T)))
(define %GET-TYPESUM->POLYMORPHIC-ATTRIBUTES-LIST
  (lambda (T)
    (hash-ref ~TYPESUM->POLYMORPHIC-ATTRIBUTES-LIST T)))

(define %GET-TYPEPROD->PARAMETER-LIST-COUNT
  (lambda (T)
    (PARAMETER-LIST:COUNT
     (hash-ref ~TYPEPROD->PARAMETER-LIST T))))
(define %GET-TYPEPROD->PARAMETER-LIST-TYPES
  (lambda (T)
    (PARAMETER-LIST:TYPES
     (hash-ref ~TYPEPROD->PARAMETER-LIST T))))
(define %GET-TYPEPROD->PARAMETER-LIST-TYPE-OPT
  (lambda (T)
    (PARAMETER-LIST:TYPE-OPT
     (hash-ref ~TYPEPROD->PARAMETER-LIST T))))
(define %GET-TYPEPROD->PARAMETER-LIST-IDS
  (lambda (T)
    (PARAMETER-LIST:IDS
     (hash-ref ~TYPEPROD->PARAMETER-LIST T))))

(define %GET-CONSTRUCTOR->MOD
  (lambda (constructor)
    (hash-ref ~TYPE->MODULE (%GET-CONSTRUCTOR->TYPE constructor) ) ) )
(define %GET-CONSTRUCTOR->TYPE
  (lambda (constructor)
    (hash-ref ~CONSTRUCTOR->TYPE constructor)))
(define %GET-CONSTRUCTOR->PARAM-TYPES
  (lambda (constructor)
    (hash-ref ~CONSTRUCTOR->PARAM-TYPES constructor)))
(define %GET-CONSTRUCTOR->PARAM-IDS
  (lambda (constructor)
    (hash-ref ~CONSTRUCTOR->PARAM-IDS constructor)))
(define %GET-CONSTRUCTOR->PARAM-COUNT
  (lambda (constructor)
    (hash-ref ~CONSTRUCTOR->PARAM-COUNT constructor)))

