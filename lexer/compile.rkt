;; -*- mode:scheme ; buffer-read-only:t -*-
#lang racket

(include "../asdl/tree.rkt")
(include "../lexer/regex.code")

(include "../lexer/grammar-file.rkt")
(include "../lexer/rexal.rkt")
(include "../lexer/data.rkt")
(include "../lexer/nfa.rkt")

(include "../tools/tools.rkt")
(include "../tools/diagnostic.rkt")
(include "../tools/output.rkt")

;;; DETERMINISTIC AUTOMATON
(define resolve-markers
  (lambda (nshash ret)

    (define m
      (apply append
             (map NFANODE/MARKERS (hash-ref nfa-set-pool nshash))))
    
    (define exit-code          'NIL)
    (define send-start-markers 'NIL)
    (define send-end-markers   'NIL)

    (define split-markers
      (lambda (m exit-col send-start-col send-end-col)
        (cond ((null? m)
               (exit-col '())
               (send-start-col '())
               (send-end-col '()))
              ((eq? 'EXIT (caar m))
               (split-markers (cdr m)
                              (lambda (e)
                                (exit-col (cons (cdar m) e)))
                              send-start-col
                              send-end-col))
              ((eq? 'SEND-START (caar m))
               (split-markers (cdr m)
                              exit-col
                              (lambda (s)
                                (send-start-col (cons (cdar m) s)))
                              send-end-col))
              ((eq? 'SEND-END (caar m))
               (split-markers (cdr m)
                              exit-col
                              send-start-col
                              (lambda (s)
                                (send-end-col (cons (cdar m) s)))))
              (else
               (PANIC "unknown marker" (car m))
               (exit 1)))))

    (split-markers m
                   (lambda (e)
                     (or (null? e)
                         (let ((code
                                (cdar
                                 (sort
                                  e
                                  (lambda (a b)
                                    (< (car a) (car b)))))))
                           (set! exit-code code))))
                   (lambda (s)
                     (or (null? s)
                         (set! send-start-markers (set->list (list->set s)))))
                   (lambda (s)
                     (or (null? s)
                         (set! send-end-markers (set->list (list->set s))))))
    (ret exit-code send-start-markers send-end-markers)))

(define dfa-set-pool
  (make-hasheqv))

(define mkdfanode
  (lambda (idx nshash)
    (define edges (make-hash))
    (define exit-code  'NIL)
    (define send-end   'NIL)
    (define send-start 'NIL)

    (define dfanode
      (lambda (m)
        (case m
          ('HASH     nshash)
          ('SET/EDGE set-edge)
          ('IDX      idx)
          ('E-MARK   exit-code)
          ('SS-MARK  send-start)
          ('SE-MARK  send-end)
          ('EDGES    edges)
          ('IS-DFA-NODE? 'YES)
          (else
           (error "mkdfanode: unknown message" m)))))

    (resolve-markers
     nshash
     (lambda (e ss se)
       (set! exit-code   e)
       (set! send-start  ss)
       (set! send-end    se)))
    
    ;; (__d "@@" idx (map NFANODE/ID (hash-ref nfa-set-pool nshash)))

    (define set-edge
      (lambda (input to)
        (and (hash-ref edges input false)
             (begin
               (PANIC "DFA / multiple transitions for the same input")
               (PANIC idx
                      ";" exit-code
                      ";" send-start
                      ";" send-end
                      ";" input
                      ":" (DFANODE/GET/IDX to)
                      ";" (DFANODE/GET/IDX (hash-ref edges input)))
               (exit 1)))
        (hash-set! edges input to)))

    dfanode))
(define DFANODE/GET/HASH
  (lambda (node)
    (node 'HASH)))
(define DFANODE/GET/IDX
  (lambda (node)
    (node 'IDX)))
(define DFANODE/GET/EDGES
  (lambda (node)
    (node 'EDGES)))
(define DFANODE/GET/EXIT/MARK
  (lambda (node)
    (node 'E-MARK)))
(define DFANODE/GET/SEND-START/MARK
  (lambda (node)
    (node 'SS-MARK)))
(define DFANODE/GET/SEND-END/MARK
  (lambda (node)
    (node 'SE-MARK)))
(define DFANODE/SET/EDGE
  (lambda (n in to)
    ((n 'SET/EDGE) in to)))
(define DFANODE/CMP
  (lambda (this that)
    ((this 'CMP) that)))
;;; dfa debugger
(define DFA/PRINT
  (lambda (START)
    (define edge-counter 0)
    (define visited (mutable-set))
    
    (define iter
      (lambda (l next-visit)
        (cond ((and (null? l) (null? next-visit))
               (__d "___")
               (__d "total visited nodes" (set-count visited))
               (__d "total edges"         edge-counter)
               (__d "\n\n\n"))
              ((null? l)
               (iter next-visit '()))
              ((not (set-member? visited (DFANODE/GET/IDX (car l))))
               (let ((NODE (car l)))
                 (let ((ID    (DFANODE/GET/IDX NODE))
                       (EDGES (DFANODE/GET/EDGES NODE))
                       (EMARK (DFANODE/GET/EXIT/MARK NODE))
                       (SSMARK (DFANODE/GET/SEND-START/MARK NODE))
                       (SEMARK (DFANODE/GET/SEND-END/MARK NODE)))
                   (set-add! visited ID)
                   (__d (__fcol 40 'right "." "__") ID "__")
                   (__d (map NFANODE/ID
                             (hash-ref nfa-set-pool
                                       (DFANODE/GET/HASH NODE))))
                   (define edges2
                     (if (null? EDGES)
                         EDGES
                         (hash-map
                          EDGES
                          (lambda (input to)
                            (set! edge-counter (add1 edge-counter))
                            (__d "\t\t"
                                 (__fcol 4 'left " "
                                         (if (< input 32)
                                             (~a ":" input)
                                             (integer->char input)))
                                 "=>" (DFANODE/GET/IDX to))
                            to))))
                   (or (eq? 'NIL EMARK)
                       (__d "EXIT" EMARK))
                   (or (eq? 'NIL SSMARK)
                       (__d "SEND START" SSMARK))
                   (or (eq? 'NIL SEMARK)
                       (__d "SEND END" SEMARK))

                   (iter (cdr l)
                         (append next-visit edges2)))))
              (else
               (iter (cdr l) next-visit)))))
    (__d ".PRINT DFA")
    (iter (list START) '())))

;;; SUBSET CONSTRUCTION
(define dfa/node/generator
  (lambda ()
    (let ((dfa/node/table (make-hasheqv)))
      (counter/reset 'DFA-NODE 0)
      (lambda (nshash ret)
        (hash-ref nfa-set-pool nshash
                  (lambda ()
                    (PANIC "bad use of DFA/NODE/GENERATOR")
                    (exit 1)))

        (cond ((hash-ref dfa/node/table nshash false) =>
               (lambda (NODE)
                 (ret NODE false)))
              (else
               (let* ((newnode (mkdfanode (1+ 'DFA-NODE) nshash)))
                 (hash-set! dfa/node/table nshash newnode)
                 (ret newnode true))))))))

(define subset-construction
  (lambda (startnode ret)
    (define dfanode (dfa/node/generator))
    
    (define iter
      (lambda (node-list NEW*)
        (if (null? node-list)
            (or (null? NEW*)
                (iter NEW* (list)))
            (let* ((DFA-ND-FROM (car node-list)))
              ;; (or (eq? 'YES (DFA-ND-FROM 'IS-DFA-NODE?))
              ;;     (error ".SUBSET-CONSTRUCTION. bad use of ITER"))
              (define SUBSET (hash-ref nfa-set-pool
                                       (DFANODE/GET/HASH DFA-ND-FROM)))
              
              (define xxx (make-hash))
              (define E (MK/TRANSITIONS xxx))
              
              (for-each
               (lambda (NFA-ND)
                 (hash-for-each
                  (NFANODE/TRANSITIONS NFA-ND)
                  (lambda (input to)
                    (for-each (lambda (nd)
                                (E nd input))
                              (hash-ref nfa-set-pool to)))))
               SUBSET)
              (hash-for-each xxx
                             (lambda (input TO)
                               (dfanode TO
                                        (lambda (TO* new?)
                                          (DFANODE/SET/EDGE DFA-ND-FROM input TO*)
                                          (and new? (set! NEW* (cons TO* NEW*)))))))
              (iter (cdr node-list) NEW*)))))

    (define START-IDX
      (NFANODE/ID startnode))
    
    (hash-set! nfa-set-pool
               (vector-ref primes START-IDX)
               (list (hash-ref node-table START-IDX)))

    ;; (__d ".SUBSET-CONSTRUCTION")
    (dfanode (vector-ref primes START-IDX)
             (lambda (startdfanode _)
               (iter (list startdfanode) (list))
               ;; (set! nfa-set-pool 'NIL)
               (ret startdfanode)))))

;;; DFA AS TABLE
(define dfa->transition-table
  (lambda (dfastart ret)

    (define TOTAL/NODES      (add1 (counter/get 'DFA-NODE)))
    (define EXIT-TABLE       (make-vector TOTAL/NODES 'NIL))
    (define SS-TABLE         (make-vector TOTAL/NODES 'NIL))
    (define SE-TABLE         (make-vector TOTAL/NODES 'NIL))
    (define TRANSITION-TABLE (make-vector TOTAL/NODES 'NIL))

    (define visited (mutable-set))
    (define edge-counter 0)

    (define iter
      (lambda (l next-visit)
        (cond ((and (null? l) (null? next-visit))
               (__d "total nodes" (set-count visited))
               (__d "total edges" edge-counter))
              ((null? l)
               (iter next-visit '()))
              ((not (set-member? visited (DFANODE/GET/IDX (car l))))
               (let ((NODE (car l)))
                 (let ((ID     (DFANODE/GET/IDX             NODE))
                       (EDGES  (DFANODE/GET/EDGES           NODE))
                       (EMARK  (DFANODE/GET/EXIT/MARK       NODE))
                       (SSMARK (DFANODE/GET/SEND-START/MARK NODE))
                       (SEMARK (DFANODE/GET/SEND-END/MARK   NODE)))

                   (set-add! visited ID)

                   (define TTABLE (make-character-set false))

                   (vector-set! TRANSITION-TABLE ID TTABLE)
                   (vector-set! EXIT-TABLE       ID EMARK)
                   (vector-set! SS-TABLE         ID SSMARK)
                   (vector-set! SE-TABLE         ID SEMARK)

                   (define edges2
                     (hash-map
                      EDGES
                      (lambda (input to)
                        (set! edge-counter (add1 edge-counter))
                        (vector-set! TTABLE input (DFANODE/GET/IDX to))
                        to)))

                   (iter (cdr l)
                         (append next-visit edges2)))))
              (else
               (iter (cdr l) next-visit)))))

    (iter (list dfastart) '())
    
    (define make-unique
      (lambda (table)
        (map
         (lambda (x)
           (if (symbol? x)
               x
               (set->list
                (list->set
                 (map car x)))))
         (vector->list table))))
    
    (ret (DFANODE/GET/IDX dfastart)
         TRANSITION-TABLE
         EXIT-TABLE
         (make-unique SS-TABLE)
         (make-unique SE-TABLE))))

(define mkdfa
  (lambda (START return)
    (subset-construction
     START
     (lambda (dfa-start-node)
       ;; (DFA/PRINT dfa-start-node)
       (dfa->transition-table dfa-start-node return)))))

;;; MAIN

(define main
  (lambda (bnf-file out-grammar-file)
    (__d "Compiling the grammar" "`" out-grammar-file "`.")
    (make-nfa
     bnf-file
     (lambda (START)
       (mkdfa START
              (save-automaton out-grammar-file))))))


(let ((args (current-command-line-arguments)))
  (or (and (= (vector-length args) 2)
           (let ((bnf (vector-ref args 0))
                 (grammar (vector-ref args 1)))
             (main bnf grammar)))
      (__p "BNF Compiler\n"
           "Use it so: ``racket <bnf-file> <output-grammar-file>``\n")))


