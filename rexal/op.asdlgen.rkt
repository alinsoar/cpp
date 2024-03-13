#lang racket ;; -*- mode:scheme ; buffer-read-only:nil -*-

;;; Given some tree in lisp format, generate the tree implementation
;;; INPUT.DEF => OUTPUT.TREE

;;; This is a generic logic for interface that provides:

;;; MODULE.CONSTRUCTOR
;;; MODULE.CONSTRUCTOR?
;;; MODULE.type?
;;; MODULE.type.polymorphic_attribute
;;; 'TREE    message for debug
;;; 'CONSID     ~    to get the constructor's name
;;; 'TYCK       ~    to check the types of parameters
;;; 'CONSCK     ...?... never used

(include "../tools/output.rkt")
(include "../asdl/asdldef-interface.rkt")

;;;                           FORMATTING and other HELPERS
;;; ============================================================

(define f/param
  (lambda (id)
    (string->symbol
     (string-append "p:" (symbol->string id)))))

(define f/anonymous-param
  (lambda (x)
    (string->symbol
     (string-append "anon:" (number->string x)))))

(define f/pretty
  (lambda (X)
    (string-append " "
                   (substring (pretty-format X #:mode 'print)
                              1))))

(define f/consid
  (lambda (constructor)
    (string->symbol
     (string-append
      (symbol->string (%GET-MODULE)) "." (symbol->string constructor)))))

(define f/cons?
  (lambda (constructor)
    (string->symbol
     (string-append (symbol->string (f/consid constructor)) "?"))))

(define f/selector
  (lambda (constructor param)
    (string->symbol
     (string-append (symbol->string (f/consid constructor))
                    "."
                    (symbol->string param)))))

(define f/module.type?
  (lambda (mod T)
    (string->symbol
     (string-append (symbol->string mod) "." (symbol->string T) "?"))))

(define f/polymorphic-attr
  (lambda (T name)
    (string->symbol
     (string-append (symbol->string (%GET-TYPE->MODULE T))
                    "."
                    (symbol->string T)
                    "."
                    (symbol->string name)))))

(define select/type/check/function
  (lambda (ty/opt)
    (case ty/opt
      ('*   ':list)
      ('?   ':opt)
      (else ':))))

;;;                            MARSHAL OUTPUT REPRESENTATIONS
;;; ============================================================

(define MARSHAL#CONSTRUCTOR?
  (lambda (constructor)
    (let* (($cons? (f/cons? constructor))
           ($consid constructor)
           ($par (MARSHAL#CONSTRUCTOR-PARAMETERS constructor))
           (select (if (null? $par) 'obj '(car obj))))
      (if (null? $par)
          `(define ,$cons?
            (lambda (obj)
              (eq? obj ',$consid)))
          `(define ,$cons?
            (lambda (obj)
              (and (cons? obj)
                   (eq?  (car obj) ',$consid))))))))

(define MARSHAL#SUM-TYPE?
  (lambda (T)
    (let ((mod (%GET-TYPE->MODULE T)))
      `(define ,(f/module.type? mod T)
        (lambda (obj)
          (and (is/sum/type? obj)
               (procedure? obj)
               ((obj 'TYCK) ',T)))))))

(define MARSHAL#CONSTRUCTOR-PARAMETERS
  (lambda (constructor)
    (define param/count (%GET-CONSTRUCTOR->PARAM-COUNT constructor))
    (define param/ids (%GET-CONSTRUCTOR->PARAM-IDS constructor))
    (MARSHAL#PARAMETERS param/count param/ids)))

(define MARSHAL#PARAMETERS
  (lambda (param/count param/ids)
    (map (lambda (id k)
           (cons k id))
         param/ids
         (range param/count))))

(define MARSHAL#TYPE-CHECKER-CALLS
  (lambda (types tyop args)
    
    (map
     (lambda (t o a)
       (let ((mod (%GET-TYPE->MODULE t)))
         (list (select/type/check/function o)
               t a)))
     types
     tyop
     args )  ) )

(define MARSHAL#CONSTRUCTOR-TYPE-CHECKER-CALLS
  (lambda (constructor)
    ;; useful, as racket does not have a type checker activated.
    (let ((types (%GET-CONSTRUCTOR->PARAM-TYPES constructor))
          (params (MARSHAL#CONSTRUCTOR-PARAMETERS constructor)))
      (MARSHAL#TYPE-CHECKER-CALLS (map car types)
                                  (map cdr types) 
                                  params) ) ) )

(define MARSHAL#TREE-REPRESENTATION
  (lambda (constructor)
    (let ((params (MARSHAL#CONSTRUCTOR-PARAMETERS constructor)))
      (if (null? params)
          '@KONS
          `(cons @KONS
            (map tree-repr (list . ,params) ) ) ) ) ) )

(define MARSHAL#SELECTORS
  (lambda (constructor)
    (let ((params0 (%GET-CONSTRUCTOR->PARAM-IDS constructor))
          (params (MARSHAL#CONSTRUCTOR-PARAMETERS constructor)))
      (filter
       (lambda (a) "do not build selectors for anonymous params" a)
       (map (lambda (p0 p)
              (and p0
                   (let ((selname (f/selector constructor p0)))
                     (list (list 'quote selname) p))))
            params0
            params)))))

(define MARSHAL#POLYMORPHIC-ATTRIBUTE-SELECTORS
  (lambda (constructor)
    (define T (%GET-CONSTRUCTOR->TYPE constructor))
    (define POLYATTR (map car (%GET-TYPESUM->POLYMORPHIC-ATTRIBUTES-LIST T)))
    (filter
     (lambda (a) a)
     (map
      (lambda (a)
        (and a
             `(',(f/polymorphic-attr T a) ((s s) ',(f/selector constructor a)))))
      POLYATTR))))

(define MARSHAL#SELECTOR/CALLS
  (lambda (constructor)
    (let ((params (%GET-CONSTRUCTOR->PARAM-IDS constructor))
          (sel (MARSHAL#SELECTORS constructor)))
      (map (lambda (p)
             `(define ,(cadar p)
               (lambda (obj)
                 (obj ',(cadar p)))))
           sel))))

(define MARSHAL#CONSTRUCTOR/INIT
  (lambda (constructor)
    (let (($type (%GET-CONSTRUCTOR->TYPE constructor))
          ($params (MARSHAL#CONSTRUCTOR-PARAMETERS constructor))
          ($tyck (MARSHAL#CONSTRUCTOR-TYPE-CHECKER-CALLS constructor))
          ($tree (MARSHAL#TREE-REPRESENTATION constructor))
          ($consid (f/consid constructor) )
          ($cons-parameters (MARSHAL#CONSTRUCTOR-PARAMETERS constructor)))
      `(define ,$consid
        (lambda ,$params
          (define @TYPE ',$type)
          (define @KONS ',$consid)
          (define ,constructor
            (lambda (self)
              (lambda ,$cons-parameters
                ,$tyck
                self)))
          (define $is-a? (lambda (self) (lambda (T) (eq? T @TYPE))))
          (define $kons? (lambda (self) (lambda (x) (eq? x @KONS))))
          (define $tree (lambda (self) ,$tree)) 
          
          ((lambda (self)
             ((,constructor (self self)) . ,$params))
           (lambda (s)
             (lambda (m)
               (case m
                 ('CONSID   ',$consid)
                 ('TYCK     ($is-a? (s s)))
                 ('TREE     ($tree  (s s)))
                 ('CONSCK   ($kons? (s s) ) )
                 .
                 ,(append (MARSHAL#SELECTORS constructor)
                          (MARSHAL#POLYMORPHIC-ATTRIBUTE-SELECTORS constructor)
                          '((else (error "unknown messagse sent to"
                                         @KONS ":" m) ) ) ) ) ) ) ) ) ) ) ) )

(define MARSHAL#CONSTRUCTOR
  (lambda (constructor)
    (let* (($type (%GET-CONSTRUCTOR->TYPE constructor))
           ($params (MARSHAL#CONSTRUCTOR-PARAMETERS constructor))
           ($params0 (map cdr $params))
           ($tyck (MARSHAL#CONSTRUCTOR-TYPE-CHECKER-CALLS constructor))
           ($consid (f/consid constructor) )
           ($cons-parameters (MARSHAL#CONSTRUCTOR-PARAMETERS constructor))
           ($kons `',constructor)
           ($kons (if (null? $params)
                      $kons
                      `(lambda ,$params0
                        (list . ,(cons $kons $params0))))))
      (list ;; $consid
            ;; $type 
            ;; $params
            ;; $tyck
            
            `(define ,$consid ,$kons)
            ) ) ) )

(define MARSHAL#TYPE-POLY-ATTRIBUTES
  (lambda (T a _)
    (if a
        (let ((mod (%GET-TYPE->MODULE T)))
          `(define ,(f/polymorphic-attr T a)
            (lambda (obj)
              (or (,(f/module.type? mod T) obj)
                  (error "poly-attribute" ',T ',a))
              (obj ',(f/polymorphic-attr T a)))))
        '())))

(define ITERATOR#CONSTRUCTOR-LIST
  (lambda (T f)
    (for-each f (%GET-TYPESUM->CONSTRUCTOR-ID-LIST T))))

;;;                                      EMIT OUTPUT FILE
;;; ============================================================

(define EMIT#HEADER
  (lambda ()
    (display ";; -*- mode:scheme ; buffer-read-only:t -*-")
    (newline)
    (newline)
    (display ";; TREE ")
    (display (%GET-MODULE))
    (display " -- this file was automatically generated by asdlgen")
    (newline)
    ))

(define EMIT#CONSTRUCTOR
  (lambda (constructor)
    (for-each
     (lambda (a)
       (display a)
       (newline))
     (MARSHAL#CONSTRUCTOR constructor))
    ;; (newline)
    ;; (for-each
    ;;  (lambda (s)
    ;;    (display (f/pretty s))
    ;;    (newline))
    ;;  (MARSHAL#SELECTOR/CALLS constructor))
    (display (MARSHAL#CONSTRUCTOR? constructor))
    (newline)
    (newline)))

(define EMIT#SUM-TYPE-POLYMORPHIC-ATTRIBUTES
  (lambda (T)
    (define POLY/ATTR (%GET-TYPESUM->POLYMORPHIC-ATTRIBUTES-LIST T))
    (display ";;; selectors for polymorphic attributes")
    (newline)
    (for-each 
     (lambda (a)
       (display (f/pretty (MARSHAL#TYPE-POLY-ATTRIBUTES T (car a) (cdr a))))
       (newline))
     POLY/ATTR)
    (newline)))

(define EMIT#TYPE-SUM
  (lambda (T)
    (newline)
    (display ";;; ")
    (display T)
    (newline) (ITERATOR#CONSTRUCTOR-LIST T EMIT#CONSTRUCTOR)
    ;; (display  (f/pretty (MARSHAL#SUM-TYPE? T)))
    ;; (newline) (EMIT#SUM-TYPE-POLYMORPHIC-ATTRIBUTES T)
    (newline)))

(define EMIT#TREE-IMPLEMENTATION
  (lambda (OUT-FILE)
    (with-output-to-file OUT-FILE
      (lambda ()
        (EMIT#HEADER)
        (for-each
         (lambda (T)
           (if (eq? 'TYPE-SUM (%GET-TYPE->KIND T))
               (EMIT#TYPE-SUM T)
               (error "LOOP#TYPE-NAME-LIST")))
         (%GET-TYPE-NAME-LIST)))
      #:mode   'binary
      #:exists 'truncate/replace)))

;;;                                             MAIN LOOP
;;; ============================================================

(define tree/dbg
  (lambda (tree)
    (map (lambda (T)
           (__d ":" (TYPE:GET-KIND T)
                (TYPE:GET-NAME T))
           (and (TYPESUM:GET-CONSTRUCTORS T)
                (map (lambda (a)
                       (__d "......"
                            (CONS:GET-ID a)
                            (CONS:GET-PARAMETER-COUNT a)
                            (CONS:GET-PARAMETER-TYPES a)
                            (CONS:GET-PARAMETER-TYPE-OPT a)
                            (CONS:GET-PARAMETER-IDS a)
                            
                            
                            ))
                     (TYPESUM:GET-CONSTRUCTORS T)))
           (and (TYPE:GET-ATTRIBUTES T)
                (__d "______ attr" (TYPE:GET-ATTRIBUTES T)))
           (__d))
         (TREE:GET-TYPE-LIST tree))))

(define driver
  (lambda (OUT-FILE)
    ;; (tree/dbg tree)
    (EMIT#TREE-IMPLEMENTATION OUT-FILE)
    
    ))

(let ((args (current-command-line-arguments)))
  "Generate the implementation of a tree from ASDL description."
  (or (and (= (vector-length args) 2)
           (file-exists? (vector-ref args 0)))
      (begin (__d "Use it so:"
                  "\tracket asdlgen.rkt INPUT-FILE.DEF OUTPUT-FILE.TREE\n"
                  "abort.")
             (newline)
             (exit 1)))
  (pretty-print-columns 80)
  (asdl-def-load (vector-ref args 0))
  (driver (vector-ref args 1))
  'DONE)

