;; -*- mode:scheme ; buffer-read-only:nil -*-

(define lexical-environment '())

(define load-bnf-file!
  (lambda (bnf-file)
    (for-each (lambda (a)
                (set! lexical-environment
                      (cons (cons (cadr a) (caddr a))
                            lexical-environment))
                ;; (__d "::" (cons (cadr a) (caddr a)))
                )
              (file->list bnf-file))))

(define primes
  (if (file-exists? "../lexer/primes")
      (file->value "../lexer/primes")
      (error "use mkprimes to generate the `primes` file first")))

;;; NONDETERMINISTIC AUTOMATON

(define nfa-set-pool
  (make-hasheqv))

(define node-table
  (make-hasheqv))

(define epsilon-closure-table
  (make-hasheqv))

(define MK/TRANSITIONS
  (lambda (EDGES)
    (lambda (to char)
      (define NEWEDGEHASH (vector-ref primes (NFANODE/ID to)))
      (define OLDEDGEHASH (hash-ref EDGES char 1))

      (or (zero? (remainder OLDEDGEHASH NEWEDGEHASH))
          (let* ((NEWHASHVAL (* NEWEDGEHASH OLDEDGEHASH)))
            (hash-ref! nfa-set-pool
                       NEWHASHVAL
                       (lambda ()
                         (cons to
                               (hash-ref nfa-set-pool OLDEDGEHASH
                                         (lambda ()
                                           (or (= OLDEDGEHASH 1)
                                               (error "SET POOL"))
                                           (list))))))
            (hash-update! EDGES
                          char
                          (lambda _ NEWHASHVAL)
                          'dummy))))))

;;; CONSTRUCTOR,  SETTER
(define MKNODE
  (lambda ()
    (define ID (1+ 'NFA-NODE))
    (define edges0 (make-hash))
    (define epsilon-edges (list))
    (define markers '())

    (define add-edge
      (MK/TRANSITIONS edges0))

    (define add-epsilon
      (lambda (to)
        (set! epsilon-edges
              (cons to epsilon-edges))))

    (define get-transitions
      (lambda ()
        edges0))

    (define get-epsilon
      (lambda ()
        epsilon-edges))

    (define SET-MARK!
      (lambda (m)
        (set! markers (cons m markers))))
    
    (define UPDATE-MARK!
      (lambda (m)
        (set! markers m)))

    (define set-mark-exit
      (lambda (code)
        (SET-MARK! (cons 'EXIT code))))

    (define set-mark-send-start
      (lambda (name i)
        (SET-MARK!
         (cons 'SEND-START
               ;; (string->symbol (~a name ":" i))
               (cons name i)))))

    (define set-mark-send-end
      (lambda (name i)
        (SET-MARK!
         (cons 'SEND-END
               ;; (string->symbol (~a name ":" i))
               (cons name i)))))

    (define get-markers
      (lambda ()
        markers))

    (define clean-epsilon
      (lambda ()
        (set! epsilon-edges 'NIL)))

    (define repr
      (lambda ()
        (list ID
              (hash-map
               edges0
               (lambda (in to)
                 (cons in (hash-ref nfa-set-pool to))))
              epsilon-edges
              markers)))

    ((lambda (nfanode)
       (hash-set! node-table ID nfanode)
       nfanode)
     (lambda (m)
       (case m
         ('ID             ID)
         ('ADD-EDGE       add-edge)
         ('EPSILON        add-epsilon)
         ('GET-EPSILON    get-epsilon)
         ('MKEXIT         set-mark-exit)
         ('SEND-START     set-mark-send-start)
         ('SEND-END       set-mark-send-end)
         ('UPDATE-MARK    UPDATE-MARK!)
         ('GET-MARK       get-markers)
         ('TRANS          get-transitions)
         ('REMOVE-EPSILON clean-epsilon)
         ('REPR           repr)
         (else
          (PANIC "MKNODE: unknown message" m)
          (exit 1)))))))
(define MKEDGE
  (lambda (n1 n2 char)
    ((n1 'ADD-EDGE) n2 char)))
(define MKEPSILON
  (lambda (n1 n2)
    ((n1 'EPSILON) n2)))
(define MKEXITCODE
  (lambda (n code)
    ((n 'MKEXIT) code)))
(define NFANODE/UPDATE/MARKERS
  (lambda (n m)
    ((n 'UPDATE-MARK) m)))
;;; GETTER
(define NFANODE/TRANSITIONS
  (lambda (n)
    ((n 'TRANS))))
(define NFANODE/MARKERS
  (lambda (n)
    ((n 'GET-MARK))))
(define NFANODE/GETEPSILONTRANS
  (lambda (n)
    ((n 'GET-EPSILON))))
(define NFANODE/ID
  (lambda (n)
    (n 'ID)))
(define NFANODE/CLEAN/EPSILON
  (lambda (n)
    ((n 'REMOVE-EPSILON))))
(define NFANODE/REPR
  (lambda (n)
    ((n 'REPR))))
;;; NFA DEBUGGER
(define NFA/PRINT
  (lambda (START)
    (define edge-counter 0)
    (define epsilon-edge-counter 0)
    (define visited (mutable-set))

    (define iter
      (lambda (l next-visit)
        (cond ((and (null? l) (null? next-visit))
               (__d "___")
               (__d "total visited nodes" (set-count visited))
               (__d "total epsilon-edges" epsilon-edge-counter)
               (__d "total edges"         edge-counter)
               (__d "\n\n\n"))
              ((null? l)
               (iter next-visit '()))
              ((not (set-member? visited (NFANODE/ID (car l))))
               (let ((NODE (NFANODE/REPR (car l))))
                 (let ((ID (car NODE))
                       (EDGES (cadr NODE))
                       (EPSILON (caddr NODE))
                       (MARK (cadddr NODE)))
                   (set-add! visited ID)
                   (__d (__fcol 40 'right "." "NODE") ID)
                   (or (null? EDGES)
                       (begin
                         (__d "EDGES")
                         (map (lambda (e)
                                (set! edge-counter (add1 edge-counter))
                                (__d "\t\t"
                                     (if (< (car e) 32)
                                         (car e)
                                         (integer->char (car e)))
                                     "=>" (map NFANODE/ID (cdr e))))
                              EDGES)))
                   (or (null? EPSILON)
                       (eq? 'NIL EPSILON)
                       (begin
                         (set! epsilon-edge-counter
                               (add1 epsilon-edge-counter))
                         (__d "EPSILON" (map NFANODE/ID EPSILON))))
                   (or (null? MARK)
                       (__d "MARK" MARK))

                   (iter (cdr l)
                         (append next-visit
                                 (if (eq? 'NIL EPSILON)
                                     '()
                                     EPSILON)
                                 (if (null? EDGES)
                                     EDGES
                                     (apply append (map cdr EDGES))))))))
              (else
               (iter (cdr l) next-visit)))))
    (__d "PRINT NFA")
    (iter (list START) '())))

;;; NFA -- CLEAN EPSILON TRANSITIONS
(define compute-epsilon-closure
  (lambda ()
    
    (define CLOSURE (make-vector
                     (add1 (hash-count node-table)) #f))
    (define IDX (list))

    (define eclose
      (lambda (border next-border)
        (cond ((and (null? border) (null? next-border))
               IDX)
              ((null? border)
               (eclose next-border (list)))
              ((< (caar border) (hash-count epsilon-closure-table))
               ;; memoizer
               (for-each
                (lambda (a)
                  (or (vector-ref CLOSURE (a 'ID))
                      (begin
                        (set! IDX (cons a IDX))
                        (vector-set! CLOSURE (a 'ID) #t))))
                (hash-ref epsilon-closure-table
                          (caar border)))
               (eclose (cdr border) next-border))
              (else
               (for-each
                (lambda (a)
                  (define a/id (a 'ID))
                  (or (vector-ref CLOSURE a/id)
                      (begin
                        (vector-set! CLOSURE a/id #t)
                        (set! IDX (cons a IDX))
                        (set! next-border
                              `((,a/id . ,a) . ,next-border)))))
                (NFANODE/GETEPSILONTRANS (cdar border)))
               (eclose (cdr border) next-border)))))

    (define compute-markers
      (lambda (i)
        (define MARK
          (apply append
                 (map NFANODE/MARKERS
                      (hash-ref epsilon-closure-table i))))
        (NFANODE/UPDATE/MARKERS (hash-ref node-table i) MARK)))

    ;; COMPUTE EPSILON CLOSURE
    (for-each
     (lambda (i)
       (vector-set! CLOSURE i #t)
       (set! IDX (list (hash-ref node-table i)))
       (hash-set! epsilon-closure-table i
                  (eclose `((,i . ,(hash-ref node-table i)))
                          (list)))
       (compute-markers i)
       (vector-fill! CLOSURE #f)
       (set! IDX (list)))
     (range 1 (add1 (hash-count node-table))))))

(define clean-epsilon-transitions
  (lambda (NODE/FROM SUBSET)
    (for-each (lambda (TO)
                (or (= (TO 'ID) (NODE/FROM 'ID))
                    (hash-for-each
                     (NFANODE/TRANSITIONS TO)
                     (lambda (input to)
                       (for-each (lambda (NODE/TO)
                                   (MKEDGE NODE/FROM NODE/TO input))
                                 (hash-ref nfa-set-pool to))))))
              SUBSET)))

(define epsilon-nfa->nfa
  (lambda ()
    (compute-epsilon-closure)
    (for-each
     (lambda (FROM/IDX)
       (define FROM (hash-ref node-table FROM/IDX))
       (NFANODE/CLEAN/EPSILON FROM)
       (clean-epsilon-transitions
        FROM
        (hash-ref epsilon-closure-table
                  FROM/IDX
                  (λ ()
                    (PANIC "bad use of EPSILONCLOSURE")
                    (exit 1)))))
     (range 1 (add1 (hash-count node-table))))
    (set! epsilon-closure-table 'NIL)))

;;; BNF COMPILER
(define eval-atomic
  (lambda (expr ret)
    (define START (MKNODE))
    (define END (MKNODE))
    (MKEDGE START END (if (char? expr)
                          (char->integer expr)
                          expr))
    (ret START END)))

(define eval-interval
  (lambda (expr ret)
    (define START (MKNODE))
    (define END (MKNODE))
    (if (eq? expr 'ANY)
        (interval-for-each/1 (cons 0 (sub1 ALPHABET-SIZE))
                             (lambda (a) (MKEDGE START END a)))
        (interval-for-each/0
         expr
         (lambda (a)
           (MKEDGE START END
                   (char->integer a)))))
    (ret START END)))

(define global-lexical-environment 'nil)

(define lookup
  (lambda (id ret)
    "find definition of a variable in the global lexical environment"
    (cond
     ((assoc id global-lexical-environment) =>
      (lambda (v)
        (ret (cdr v))))
     (else
      (PANIC "lex/lookup/unknown variable:" id)
      (exit 1)))))

(define eval-variable
  (lambda (expr ret)
    (lookup expr
            (lambda (v)
              (eval v ret)))))

(define eval-reduce
  (lambda (expr ret)
    (define START (MKNODE))

    (define iter
      (lambda (rands)
        (if (null? rands)
            (ret START)
            (let ((reduction (caar rands))
                  (exit-code (cadar rands)))
              (define EXIT (MKNODE))
              (MKEXITCODE EXIT (cons
                                (1+ 'EXIT-CODE-PRIORITY)
                                exit-code))
              (eval reduction
                    (lambda (start* exit*)
                      ;; (__d ".REDUCTION" (__fcol 40 'right "." exit-code))
                      (MKEPSILON START start*)
                      (MKEPSILON exit* EXIT)
                      (iter (cdr rands))))))))
    (iter expr)))

(define eval-union
  (lambda (expr ret)
    "the alternative operator"

    (define START (MKNODE))
    (define END (MKNODE))
    (define iter
      (lambda (rands)
        (if (null? rands)
            (ret START END)
            (let ((rand (car rands)))
              (eval rand
                    (lambda (start* end*)
                      (MKEPSILON START start*)
                      (MKEPSILON end* END)
                      (iter (cdr rands))))))))
    (iter expr)))

(define eval-concatenation
  (lambda (expr ret)
    "the concat operator"
    (define iter
      (lambda (rands END ret)
        (if (null? rands)
            (ret END)
            (eval (car rands)
                  (lambda (start* end*)
                    (MKEPSILON END start*)
                    (iter (cdr rands) end* ret))))))
    (eval (car expr)
          (lambda (START END)
            (iter (cdr expr)
                  END
                  (lambda (end*)
                    (ret START end*)))))))

(define eval-regular-expression
  (lambda (expr ret)
    "from the regular expression representation to operators."
    (define re (re/compile (string->list expr)))
    (eval re ret)))

(define eval-repeat
  (lambda (expr ret)
    "the kleene star operator and concatenation of finite repetitions"

    (or (= 3 (length expr))
        (begin
          (PANIC "KLEENE --" expr)
          (exit 1)))

    (define min (car expr))
    (define max (cadr expr))
    (define rand (caddr expr))

    (define iter
      (lambda (kounter END ret)
        (if (zero? kounter)
            (ret END)
            (eval rand
                  (lambda (start* end*)
                    (MKEPSILON END start*)
                    (iter (sub1 kounter) end* ret))))))

    (cond
     ((and (eq? min 0)
           (eq? max 'INF))
      ;; (__d ".KLEENE" rand)
      (define START (MKNODE))
      (define END (MKNODE))
      (eval rand
            (lambda (start* end*)
              (MKEPSILON START start*)
              (MKEPSILON end* END)
              (MKEPSILON START END)
              (MKEPSILON END START)
              (ret START END))))
     ((eq? max 'INF)
      (eval rand
            (lambda (START END)
              (iter (sub1 min)
                    END
                    (lambda (end*)
                      (eval rand
                            (lambda (start** end**)
                              (MKEPSILON end* start**)
                              (MKEPSILON start** end**)
                              (MKEPSILON end** start**)
                              (ret START end**))))))))
     ((and (positive? min)
           (= min max))
      ;; (__d ".REPEAT" min rand)
      (eval rand
            (lambda (START END)
              (iter (sub1 min)
                    END
                    (lambda (end*)
                      (ret START end*))))))
     (else
      (error "kleene -- to do" min max (caddr expr))))))

(define eval-optional
  (lambda (expr ret)
    ;; (__d ".OPT" expr)
    (or (= 1 (length expr))
        (begin
          (PANIC "OPTIO --" expr)))
    (define START (MKNODE))
    (define END (MKNODE))
    (MKEPSILON START END)
    (define rand (car expr))
    (eval rand
          (lambda (start* end*)
            (MKEPSILON START start*)
            (MKEPSILON end* END)
            (ret START END)))))

(define eval-send
  (lambda (expr ret)

    (or (= 2 (length expr))
        (begin
          (PANIC "SEND --" expr)
          (exit 1)))

    (define name (car expr))
    (define rand (cadr expr))

    (define START (MKNODE))
    (define END (MKNODE))
    (define index (1+ 'SEND-DATA))

    ((START 'SEND-START) name index)
    ((END 'SEND-END) name index)

    (eval rand
          (lambda (start* end*)
            (MKEPSILON START start*)
            (MKEPSILON end* END)
            (ret START END)))))

(define eval
  (lambda (expr ret)
    "We have some abstractions in variables.  No need to dynamically
define other abstractions (analogous of groups in more general
regexps).  So the environment does not change."
    ;; (__d ".EVAL" expr)
    (cond ((char? expr)
           (eval-atomic expr ret))
          ((integer? expr)
           (eval-atomic expr ret))
          ((eq? 'ANY expr)
           (eval-interval expr ret))
          ((symbol? expr)
           (eval-variable expr ret))
          ((string? expr)
           (eval-regular-expression expr ret))
          ((not (cons? expr))
           (__d)
           (__d "EVAL.TODO --" expr)
           (__d)
           (exit 1))
          (else
           ((case (car expr)
              ('CONCAT         eval-concatenation )
              ('UNION          eval-union )
              ('INTERVAL       eval-interval )
              ('REDUCE         eval-reduce )
              ('REPEAT         eval-repeat )
              ('SEND           eval-send )
              ;; ('COMPLEMENT     eval-complement--experimental )
              ('OPT            eval-optional )
              (else
               (__d)
               (__d "EVAL.TODO --" expr)
               (__d)
               (exit 1)))
            (operands expr)
            ret)))))

(define make-nfa
  (lambda (bnf-file return)
    ;; todo -- cut global-lexical-environment
    (load-bnf-file! bnf-file)
    (set! global-lexical-environment lexical-environment)
    (lookup ':start-symbol
            (lambda (:S)
              (eval :S
                    (lambda (START)
                      ;; (NFA/PRINT START)
                      (epsilon-nfa->nfa)
                      ;; (NFA/PRINT START)
                      (return START)))))))


