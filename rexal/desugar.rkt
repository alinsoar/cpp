;; -*- mode:scheme ; buffer-read-only:t -*-
#lang racket

;;; Given some lexical.bnf file, it desugars it in the language of
;;; operator language made of combinations of the following operators

;;; -- atomic data structures: int, variable-names
;;; -- CONCAT
;;; -- UNION
;;; -- KLEENE
;;; -- SEND
;;; -- BOL

(include "all.inc")

(define ALPHABET-SIZE 128)

(define environment 'nil)

(define desugar-concatenation
  (lambda (re)
    (lambda (return)
      (define iter
        (lambda (a col)
          (if (null? a)
              (col '())
              ((desugar (car a))
               (lambda (d)
                 (iter (cdr a)
                       (lambda (rest)
                         (col (cons d rest)))))))))
      (iter re
            (lambda (e*)
              (return
               `(CONCAT . ,e*)))))))

(define desugar-repeat
  (lambda (t0 t1 re)
    (lambda (return)
      ((desugar re)
       (lambda (d)
         (cond ((and (zero? t0)
                     (eq? t1 'INF))
                (return (OP.KLEENE d)))
               ((eq? 'INF t1)
                ((desugar-repeat 0 t1 re)
                 (lambda (d0)
                   (define xx1
                     (append (map (lambda _ d) (range t0))
                             (list d0)))
                   (return `(CONCAT . ,xx1)))))
               ((zero? t0)
                (return
                 (cons 'UNION
                       (cons '(UNION)
                             (map (lambda (n)
                                    (cons 'CONCAT
                                          (map (lambda _ d)
                                               (range (add1 n)))))
                                  (range 0 t1))))))
               (else
                ((desugar-repeat 0 (- t1 t0) re)
                 (lambda (d0)
                   (define xx1
                     (append (map (lambda _ d) (range t0))
                             (list d0)))
                   (return `(CONCAT . ,xx1)))))))))))

(define desugar-union
  (lambda (re)
    (lambda (return)
      (define iter
        (lambda (a col)
          (if (null? a)
              (col '())
              ((desugar (car a))
               (lambda (d)
                 (iter (cdr a)
                       (lambda (rest)
                         (col (cons d rest)))))))))
      (iter re
            (lambda (e*)
              (return
               `(UNION . ,e*)))))))

(define desugar-send
  (lambda (sym re)
    (lambda (return)
      ((desugar re)
       (lambda (d)
         (return (OP.SEND sym d)))))))

(define desugar-beginning-of-line
  (lambda (re)
    (lambda (return)
      ((desugar re)
       (lambda (d)
         (return (OP.BOL d)))))))

(define desugar-interval
  (lambda (from to)
    (lambda (return)
      ((desugar from)
       (lambda (a)
         ((desugar to)
          (lambda (b)
            (return
             `(UNION . ,(range a b))))))))))

(define desugar-any
  (lambda (return)
    (return `(UNION . ,(range ALPHABET-SIZE)))))

(define desugar-atom
  (lambda (a)
    (lambda (return)
      (return a))))

(define desugar-optional
  (lambda (re)
    (lambda (return)
      ((desugar re)
       (lambda (d)
         (return `(UNION (UNION) ,d)))))))

(define desugar-regular-expression
  (lambda (s)
    (desugar (re/compile s))))

(define desugar-complement
  (lambda (re)
    (lambda (return)
      ((desugar re)
       (lambda (d)
         ;; (__d ".desugar.complement" re)
         (mk/epsilon/nfa
          d
          (lambda (e.nfa start0 end0)
            (epsilon/nfa->dfa
             e.nfa start0 end0
             (lambda (dfa dfa.start)
               (dfa.complement
                dfa dfa.start
                (lambda (dfa.complement co.start)
                  ;; (AUTO.DFA.DBG dfa.complement co.start) (exit 0)
                  (dfa.myhill–nerode.minimize
                   dfa.complement co.start
                   (lambda (dfa.co.min dfa.co.min.start)
                     ;; (AUTO.DFA.DBG dfa.co.min dfa.co.min.start) (exit 0)
                     (dfa->langop
                      dfa.co.min dfa.co.min.start
                      (lambda (re.complement)
                        ((desugar re.complement)
                         return))))))))))))))))

(define desugar-variable
  (lambda (re)
    (desugar (hash-ref environment re))))

(define desugar
  (lambda (re)
    (cond ((OP.ANY? re)
           desugar-any)
          ((symbol? re)
           (desugar-variable re))
          ((string? re)
           (desugar-regular-expression (string->list re)))
          ((char? re)
           (desugar-atom (char->integer re)))
          ((integer? re)
           (desugar-atom re))
          ((OP.CONCAT? re)
           (desugar-concatenation (cdr re)))
          ((OP.UNION? re)
           (desugar-union (cdr re)))
          ((OP.REPEAT? re)
           (desugar-repeat (cadr re) (caddr re) (cadddr re)))
          ((OP.SEND? re)
           (desugar-send (cadr re) (caddr re)))
          ((OP.BOL? re)
           (desugar-beginning-of-line (cadr re)))
          ((OP.INTERVAL? re)
           (desugar-interval (cadr re) (caddr re)))
          ((OP.OPT? re)
           (desugar-optional (cadr re)))
          ((OP.COMPLEMENT? re)
           (desugar-complement (cadr re)))
          (else
           (error "\ndesugar::" re)))))

(define reduce
  (lambda (startnode return)
    ((lambda (s) (s s startnode return))
     (lambda (s reductions col)
       (if (null? reductions)
           (col '())
           (let* ((R (car reductions))
                  (ID (car R)))
             ((desugar ID)
              (lambda (desugared/regexp)
                (__d  ".reduce" ID "...." 'desugared/regexp)
                ;; (pretty-print (hash-ref environment ID))
                (mk/epsilon/nfa
                 desugared/regexp
                 (lambda (e.nfa start0 end0)
                   (epsilon/nfa->dfa
                    e.nfa start0 end0
                    (lambda (dfa start)
                      (dfa.myhill–nerode.minimize
                       dfa start
                       (lambda (dfa.min dfa.min.start)
                         (dfa->langop
                          dfa.min dfa.min.start
                          (lambda (re)
                            (__d "...." (pretty-format re))
                            (__d "========================================")
                            (return re))))))))) 
                (s s (cdr reductions)
                   (lambda (r)
                     (col (cons (list ID desugared/regexp)
                                r))))))))))))

(input-output-file
 (lambda (in out)
   (set! environment (load-bnf-file in))
   (pretty-print-columns 500)
   (let ((start (hash-ref environment ':start-symbol)))
     (and start
          (reduce (cdr start)
                  (lambda (r)
                    ;; (map (lambda (r)(__d ">>" r))r)
                    (out
                     (lambda ()
                       (display 'test)
                       (newline)
                       'fine))))))))

