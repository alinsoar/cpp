#lang racket ;; -*- mode:scheme ; buffer-read-only:nil -*-

;;; Given some tree in lisp format, generate the tree implementation
;;; INPUT.DEF => OUTPUT.TREE

(include "../tools/output.rkt")
(include "../asdl/asdldef-interface.rkt")

;; ;;;                           FORMATTING and other HELPERS
;; ;;; ============================================================

(define f/insert/trie/def
  (lambda (mod ty name)
    (string->symbol
     (string-append ":" (symbol->string mod) "." (symbol->string ty) "." (symbol->string name)))))

(define f/insert/trie/def?
  (lambda (mod ty name)
    (string->symbol
     (string-append ":" (symbol->string mod) "." (symbol->string ty) "." (symbol->string name) "?"))))

(define f/insert/trie/strdef
  (lambda (ty name)
    (string->symbol
     (string-append "." (symbol->string name)))))

(define f/clex/hash/1
  (lambda (mod ty )
    (string->symbol
     (string-append (symbol->string mod) "." (symbol->string ty) ".hash"))))

(define f/clex/hash/2
  (lambda (name)
    (string->symbol
     (string-append  "CTOK." (symbol->string name)))))

;;; ********************************************************************************

(define MARSHAL#PPTRIE-INSERT-STRING
  (lambda (M T K)
    `(define ,(f/insert/trie/def M T K) (pptrie.insert-string ,(f/insert/trie/strdef T K)))))

(define MARSHAL#PPTRIE-IS-A?
  (lambda (M T K)
    `(define ,(f/insert/trie/def? M T K) (lambda (k) (= k ,(f/insert/trie/def M T K))))))

(define MARSHAL#CLEX-HASHES
  (lambda (ty ko return)
    (define MODULE (%GET-MODULE))
    (define hashname (f/clex/hash/1 MODULE ty))
    (define hashes (map
                    (lambda (a)
                      `(cons
                        ,(f/insert/trie/def MODULE ty a)
                        ,(f/clex/hash/2 a)))
                    ko))
    (return hashname hashes)))

;;; ********************************************************************************

(define EMIT#PPTRIE-INSERT-STRING
  (lambda ()
    (define MODULE (%GET-MODULE))
    
    (define MOD/DEFS
      (map (lambda (a)
             (cons a (%GET-TYPESUM->CONSTRUCTOR-ID-LIST a)))
           (%GET-TYPE-NAME-LIST)))
    
    (for-each
     (lambda (x)
       (for-each (lambda (y)
                   (display (MARSHAL#PPTRIE-INSERT-STRING MODULE (car x) y))
                   (newline))
                 (cdr x)))
     MOD/DEFS)))

(define EMIT#PPTRIE-IS-A?
  (lambda ()
    (define MODULE (%GET-MODULE))
    (define MOD/DEFS
      (map (lambda (a)
             (cons a (%GET-TYPESUM->CONSTRUCTOR-ID-LIST a)))
           (%GET-TYPE-NAME-LIST)))
    (for-each
     (lambda (x)
       (for-each (lambda (y)
                   (display (MARSHAL#PPTRIE-IS-A? (%GET-MODULE) (car x) y))
                   (newline))
                 (cdr x)))
     MOD/DEFS)))

(define EMIT#CLEX-HASHES
  (lambda ()
    (define MODULE (%GET-MODULE))
    (define MOD/DEFS
      (map (lambda (a)
             (cons a (%GET-TYPESUM->CONSTRUCTOR-ID-LIST a)))
           (%GET-TYPE-NAME-LIST)))
    (for-each
     (lambda (x)
       (and (memq (car x) '(punct key))
            (MARSHAL#CLEX-HASHES 
             (car x) (cdr x)
             (lambda (n v)
               (display "(define ")
               (display n)
               (newline)
               (display "  (make-hash")
               (newline)
               (display "    (list")
               (newline)
               (map (lambda (a) 
                      (display "      ")
                      (display a)
                      (newline))
                    v)
               (display " )))")
               (newline)
               (newline)))))
     MOD/DEFS)))

(define EMIT#HEADER
  (lambda ()
    (display ";; -*- mode:scheme ; buffer-read-only:t -*-")
    (newline)
    (display ";;; this file was autogenerated by asdlgen")
    (newline)
    (newline)))

(define driver
  (lambda (OUT-FILE)
    (with-output-to-file OUT-FILE
      (lambda ()
        (EMIT#HEADER)
        ;; (ITERATOR#TYPE-NAME-LIST EMIT#TYPE-SUM EMIT#TYPE-PRODUCT)
        (EMIT#PPTRIE-INSERT-STRING)
        (newline)
        (EMIT#PPTRIE-IS-A?)
        (newline)
        (EMIT#CLEX-HASHES)
        (newline)
        )
      #:mode   'binary
      #:exists 'truncate/replace)))

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
  (driver (vector-ref args 1)))