#lang racket ;; -*- mode:scheme ; buffer-read-only:nil -*-

;;; parses the file in asdl format and rewrites it in lisp format
;;; TREE.ASDL => TREE.DEF

(include "../lexer/data.rkt")
(include "../lexer/grammar-file.rkt")
(include "../lexer/lexer-groups.rkt")
(include "../asdl/trie.rkt")
(include "../tools/diagnostic.rkt")
(include "../tools/output.rkt")
(include "../tools/tools.rkt")

(define input-file
  (lambda (INPUT-TREE-FILE)
    (display (~a "tree file:" INPUT-TREE-FILE "\n"))
    (set! input-file INPUT-TREE-FILE)))

(define output-file-stream
  (lambda (OUTPUT-TREE-FILE)
    (display (~a "output file:" OUTPUT-TREE-FILE "\n"))
    (set! output-file-stream
          (lambda (tree)
            (with-output-to-file OUTPUT-TREE-FILE
              (lambda ()
                (print-tree tree))
              #:mode   'binary
              #:exists 'truncate/replace)))))
(define TRIE (lex_trie))
(define MKLEXER
  (lambda (start)
    (let ((asdllex (make-lexer-groups "../asdl/asdl-grammar" TRIE)))
      (lex-file-buffer input-file asdllex start))))

;;;                                     PARSE DATA
;;; ==================================================

(define iter-fields
  (lambda (data col)
    (case (and (cons? data)
               (caar data))
      ('FIELD-TYPE-ID
       (iter-fields (cdr data)
                    (lambda (x acc re)
                      (define ty (cdar data))
                      (set-add! accessed-types ty)
                      (col '()
                           (cons (cons `(FTY ,ty) x) acc)
                           re))))
      ('FIELD-ID
       (iter-fields (cdr data)
                    (lambda (x acc re)
                      (or (null? x)
                          (error "field-id:" x))
                      (col `((FID ,(cdar data)))
                           acc
                           re))))
      ('FIELD-TYPE-OPT
       (iter-fields (cdr data)
                    (lambda (x acc re)
                      (col (cons `(OPT ,(cdar data))
                                 x)
                           acc
                           re))))
      ((CONS-ID ATTRIBUTES)
       (col '() '() data))
      (else
       (or (null? data) (error "iter-fields" data))
       (col '() '() '())))))

(define accessed-types
  (mutable-set))

(define defined-types
  (mutable-set))

(define df/sum
  (lambda (tok-val start-point end-point S E SPL has/eof? ret)
    (define ty 'nil)

    (define iter
      (lambda (xx col)
        (if (null? xx)
            (col '() '())
            (let ((item (car xx)))
              (case (car item)
                ('DEF-TYPE-ID
                  (set! ty (cdr item))
                  (set-add! defined-types ty)
                  (iter (cdr xx) col))
                ('CONS-ID
                 (iter-fields
                  (cdr xx)
                  (lambda (x params rest)
                    (iter rest
                          (lambda (r attributes)
                            (or (null? x) (error "constructor:" x))
                            (define S (cdr item))
                            (col (cons (cons `(CONS ,S)
                                             (append params attributes))
                                       r)
                                 attributes))))))
                ('ATTRIBUTES
                 (iter-fields
                  (cdr xx)
                  (lambda (x attributes re)
                    (or (null? re) (error "attribute:" re))
                    (col '() attributes))))
                ('SUM
                 (iter (cdr xx) col))
                (else
                 (error "df/sum/iter" xx)))))))
    (iter tok-val
          (lambda (x attr)
            (ret (list ty (cons 'polymorphic_attributes attr) x))))))

(define df/product
  (lambda (tok-val start-point end-point S E SPL has/eof? ret)
    (define ty (cdar tok-val))
    (set-add! defined-types ty)
    (iter-fields
     (cdr tok-val)
     (lambda (x params rest)
       (or (null? x) (error "df/product: fields" x))
       (or (null? rest) (error "df/product/fields" rest))
       (ret (list ty `(((ANONYMOUS/CONS) . ,params))))))))

(define tree
  (lambda (tok-val start-point end-point S E SPL has/eof? ret)

    (define iter
      (lambda (data col)
        (define next
          (lambda (a b s)
            (s (cdar data))))

        (case (and (cons? data)
                   (caar data))
          ('DEF-TYPE-ID
            (iter (cdr data)
                  (lambda (a b)
                    (or (null? a)
                        (error "bad tree" a))
                    (next a b
                          (lambda (xx)
                            (col '() b))))))
          ('MODULE
           (iter (cdr data)
                 (lambda (a b)
                   (or (null? a)
                       (error "bad tree/mod" a))
                   (next a b
                         (lambda (xx)
                           (col '()
                                (cons 'MODULE (cons xx b))))))))
          ('IMPORT-MOD
           (iter (cdr data)
                 (lambda (a b)
                   (next a b
                         (lambda (xx)
                           (col '()
                                (cons (cons 'IMPORT-MOD (cons xx a))
                                      b)))))))
          ('IMPORT
           (iter (cdr data)
                 (lambda (a b)
                   (next a b
                         (lambda (xx)
                           (set-add! defined-types xx)
                           (col (cons `(IMPORT ,xx) a)
                                b))))))
          (else
           (or (null? data)
               (error "tree: bad data"))
           (col '() '())))))

    (iter tok-val
          (lambda (__ b)
            (ret b)))))

;;;                            PRINT AND SAVE DATA
;;; ==================================================

(define dbg-tree
  (lambda (tree)
    (display "\n@TREE@\n")
    (map (lambda (a) (display a) (newline)) tree)
    (newline)))

(define kons/kount 0)

(define write/params
  (lambda (p*)
    (map
     (lambda (p)
       (display " ")
       (display #\()
       (let ((fty (assoc 'FTY p))
             (opt (assoc 'OPT p))
             (fid (assoc 'FID p)))
         (write (cadr fty))
         (display " ")
         (write (and opt (cadr opt)))
         (display " ")
         (write (and fid (cadr fid))))
       (display #\)))
     p*)))

(define write/attributes
  (lambda (a*)
    (newline)
    (display "    ")
    (display #\()
    (display "attributes")
    (write/params a*)
    (display #\))))

(define write/polymorphic/attributes
  (lambda (polymorphic_attributes)
    (display " (polymorphic-attributes ")
    (display
     (map
      (lambda (a)
        (define TY (assoc 'FTY a))
        (define ID (assoc 'FID a))
        (cons (and ID (cadr ID))
              (and TY (cadr TY))))
      polymorphic_attributes))
    (display ")")))

(define write/cons/sum
  (lambda (CONS/DEFS ATTR/DEFS)
    (map
     (lambda (k r)
       (set! kons/kount (add1 kons/kount))
       (display "    ")
       (display #\()
       (display (cadar k))
       (write/params (cdr k))
       (display #\))
       (and (positive? r) (newline)))
     CONS/DEFS
     (range (sub1 (length CONS/DEFS)) -1 -1) )
    (or (null? ATTR/DEFS)
        (write/attributes (cdaar ATTR/DEFS)))))

(define write/cons/product
  (lambda (params)
    (display "    ")
    (display #\()
    (write/params params)
    (display #\))))

(define write/defs
  (lambda (module/defs)
    (for-each
     (lambda (df)
       (define TYPE
         (if (eq? 'polymorphic_attributes (caadr df))
             'SUM
             'PRODUCT))
       (display #\()
       (case TYPE
         ('SUM
          (define cons/list (caddr df))
          (define CONS/DEFS (filter (lambda (a) (eq? 'CONS (caar a))) cons/list))
          (define ATTR/DEFS (filter (lambda (a) (eq? 'ATTR (caar a))) cons/list))
          (define polymorphic_attributes (cdadr df))
          (display "TYPE-SUM ") (display (car df))
          (write/polymorphic/attributes polymorphic_attributes)
          (newline) (write/cons/sum CONS/DEFS ATTR/DEFS))
         ('PRODUCT
          (display "TYPE-PRODUCT ") (display (car df))
          (newline) (write/cons/product (cdaadr df))))
       (display #\))
       (newline))
     module/defs)))

(define write/import
  (lambda (m)
    (write (cadr m))))

(define write/import/mod
  (lambda (m)
    (display #\()
    (display "IMPORT")
    (display " ")
    (display (cadr m))
    (map
     (lambda (m)
       (newline) (display "        ") (write/import m))
     (cddr m))
    (display #\))))

(define write/module
  (lambda (module/tree)
    (display #\()
    (display (car module/tree))
    (display " ")
    (display (cadr module/tree))
    (map
     (lambda (m)
       (newline) (display "    ")
       (case (car m)
         ('IMPORT-MOD
          (write/import/mod m))
         (else
          (error "tree-mod" m))))
     (cddr module/tree))
    (display #\))
    ))

(define print-tree
  (lambda (tree)
    (define module/tree (assoc 'MODULE tree))
    (define module/defs
      (filter
       (lambda (d)
         (char-lower-case?
          (string-ref
           (symbol->string (car d))
           0)))
       tree))
    (display ";; TREE ")
    (display (cadr module/tree))
    (display " -- this file is automatically generated by asdl")
    (display " -*- racket -*-")
    (newline)
    (newline)
    ;; WRITE MODULE
    (write/module module/tree)
    (newline)
    ;; WRITE DEFINITIONS
    (write/defs module/defs)
    (newline)

    (display ";; total number of types ")
    (display (length module/defs))
    (newline)
    (display ";; total number of constructors ")
    (display kons/kount)

    (newline)))

;;;                                     CHECK DATA
;;; ==================================================

(define check-tree
  (lambda (tree)
    (define defs
      (filter
       (lambda (d)
         (char-lower-case?
          (string-ref
           (symbol->string (car d))
           0)))
       tree))
    ;; the referenced types are defined?
    (for-each
     (lambda (T)
       (or (set-member? defined-types T)
           (PANIC "not defined type" T)))
     (set->list accessed-types))
    ;; module check
    (or (assoc 'MODULE tree)
        (PANIC "module is not defined"))
    ;; conflicting constructor names
    (define KONS (mutable-set))
    (map (lambda (df)
           (map
            (lambda (x)
              (map
               (lambda (k)
                 (define constructor/type (caar k))
                 (and (eq? 'CONS constructor/type)
                      (let ((NAME (cadar k)))
                        (if (set-member? KONS NAME)
                            (PANIC "duplicate name for constructor" NAME)
                            (set-add! KONS (cadar k))))))
               x))
            (cddr df)))
         defs)
    ;; redefinition of types
    (define redef (mutable-set))
    (map
     (lambda (a)
       (if (set-member? redef (car a))
           (PANIC "multiple definitions for type" (car a))
           (set-add! redef (car a))))
     defs)
    ))

;;;                                     MAIN LOOP
;;; ==================================================

(define RUN
  (lambda (LEXER stream/ignored)

    ((lambda (next)
       (next next
             (lambda (tree)
               ;; (dbg-tree tree)
               (check-tree tree)
               (and (> ERROR-COUNT 0)
                    (begin
                      (display "abort")
                      (newline)
                      (exit 1)))
               (output-file-stream tree)
               'SUCCESS)))
     (lambda (next col)
       (LEXER
        (lambda (tok-type tok-uniqueid tok-val start-point end-point S E SPL has/eof?)
          (define filter-out-empty
            (filter (lambda (a) (cons? (cadr a))) tok-val))
          (define ordered-val
            (sort filter-out-empty
                  (lambda (a b) (< (caddr a) (caddr b)))))
          (define data
            (map (lambda (a)
                   (cons (car a)
                         (string->symbol
                          (list->string
                           (map integer->char (cadr a)))) ))
                 ordered-val))
          (case tok-type
            ((@comm)
             (next next col))
            ((@df)
             ((if (assoc 'SUM tok-val)
                  df/sum
                  (if (assoc 'PRODUCT tok-val)
                      df/product
                      (error "RUN:does not happen")))
              (filter (lambda (a) (not (or (eq? (car a) 'SUM)
                                      (eq? (car a) 'PRODUCT))))
                      data)
              start-point end-point S E SPL has/eof?
              (lambda (df)
                (next next
                      (lambda (r)
                        (col (cons df r)))))))
            ((@tree)
             (tree data start-point end-point S E SPL has/eof?
                   (lambda (m)
                     (next next
                           (lambda (r)
                             (col
                              (cons m r)))))))
            (else
             (next next col))))
        (lambda (co)
          "EOF"
          (col '())))))))

(let ((args (current-command-line-arguments)))
  (display "Rewrites ASDL format in lisp format")
  (newline)
  (or (and (= (vector-length args) 2)
           (file-exists? (vector-ref args 0)))
      (begin (display "Use it so:\tracket asdl.rkt INPUT-FILE OUTPUT-FILE\n")
             (display "abort.")
             (newline)
             (exit 1)))
  (input-file (vector-ref args 0))
  (output-file-stream (vector-ref args 1))
  (MKLEXER RUN))
