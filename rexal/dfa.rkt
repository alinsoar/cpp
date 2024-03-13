;; -*- mode:scheme ; buffer-read-only:nil -*-

(define primes (include "../lexer/primes"))

(define all/words/automaton
  (let ((memo #f))
    (lambda (return)
      (if memo
          (return (car memo) (cdr memo))
          ((desugar ".*")
           (lambda (d)
             (mk/epsilon/nfa
              d
              (lambda (enfa start end)
                (epsilon/nfa->dfa enfa start end
                                  (lambda (dfa start)
                                    (set! memo (cons dfa start))
                                    (return dfa start)))))))))))

(define dfa.complement.exit.states
  (lambda (dfa return)
    (define negative/exit (AUTO.DFA.COPY dfa))
    (DFA.set.exit/table negative/exit 
                        (vector-map not (DFA.exit/table dfa)))
    (return negative/exit)))

(define dfa.complement
  (lambda (dfa dfa.start return)
    ;; (__d ".. complement")
    ;; (AUTO.DFA.DBG dfa dfa.start)
    (dfa.complement.exit.states
     dfa
     (lambda (dfa/exit/complement)
       ;; (AUTO.DFA.DBG dfa/exit/complement dfa.start)
       (all/words/automaton
        (lambda (all init.all)
          ;; (AUTO.DFA.DBG all init.all)
          (dfa.product all init.all
                       dfa/exit/complement dfa.start
                       (lambda (product.dfa0 product.dfa.start.state0 complement/state)
                         ;; (AUTO.DFA.DBG product.dfa0 product.dfa.start.state0)
                         (AUTO.DFA.SETEXIT product.dfa0 complement/state)
                         (define dummy/state (AUTO.DFA.ADDNODE product.dfa0))
                         ;; ==================== make transitions
                         (for-each
                          (lambda (in)
                            (AUTO.DFA.SET.TRANSITION
                             product.dfa0 complement/state in dummy/state))
                          (range ALPHABET-SIZE))
                         ;; clean the product
                         (dfa.clean.nonreacheble.states
                          product.dfa0 product.dfa.start.state0
                          (lambda (product product/start/node)
                            ;; (AUTO.DFA.DBG product product/start/node) (exit 0)
                            (return product product/start/node)))))))))))

(define dfa.clean.nonreacheble.states
  (lambda (dfa start return)
    
    (define find-reacheble
      (lambda (nodeset visited)
        (let ((border (list->set
                       (apply append
                              (set-map
                               nodeset
                               (lambda (n)
                                 (hash-values
                                  (AUTO.DFA.GET.TRANSITION/TABLE dfa n))))))))
          (let ((diff (set-subtract border visited)))
            (if (set-empty? diff)
                visited
                (find-reacheble diff (set-union visited diff)))))))
    
    (let ((reached/nodes (find-reacheble (set start) (set start)))
          (new/dfa (AUTO.DFA 1000000))
          (hash/nodes (make-hash))
          (new/start/state 'nil))
      ;; ============================== set node-associations, exit states
      (set-for-each
       reached/nodes
       (lambda (node)
         (define new (AUTO.DFA.ADDNODE new/dfa))
         (hash-set! hash/nodes node new)
         (and (= node start)
              (set! new/start/state new))
         (and (AUTO.DFA.GET.EXIT dfa node)
              (AUTO.DFA.SETEXIT new/dfa new))
         (AUTO.DFA.SET/NFA/NODES new/dfa new (AUTO.DFA.GET.NFANODES dfa node))))
      ;; ============================== set transitions
      (set-for-each
       reached/nodes
       (lambda (node)
         (let ((edges (AUTO.DFA.GET.TRANSITION/TABLE dfa node)))
           (let ((meaningful/edges
                  (filter (lambda (x) (set-member? reached/nodes (cdr x)))
                          (hash->list edges)))
                 (from (hash-ref hash/nodes node)))
             (for-each
              (lambda (edge)
                (define input (car edge))
                (define to (hash-ref hash/nodes (cdr edge)))
                (AUTO.DFA.SET.TRANSITION new/dfa from input to))
              meaningful/edges)))))
      (return new/dfa new/start/state))))

(define dfa.product
  (lambda (dfa1 start1 dfa2 start2 return)
    (define PRODUCT (AUTO.DFA 1000000))
    (define product.index
      (lambda (node1 node2)
        (+ (* (DFA.node-count dfa1) node1) node2)))
    (define product/start/node (product.index start1 start2))
    (for-each
     (lambda (i2)
       (let ((n2 (AUTO.DFA.GET.NFANODES dfa2 i2))
             (x2 (AUTO.DFA.GET.EXIT dfa2 i2)))
         (for-each
          (lambda (i1)
            (let ((n1 (AUTO.DFA.GET.NFANODES dfa1 i1))
                  (x1 (AUTO.DFA.GET.EXIT dfa1 i1)))
              (define p/idx (product.index i2 i1))
              (AUTO.DFA.ADDNODE PRODUCT)
              (and x1 x2 (AUTO.DFA.SETEXIT PRODUCT p/idx))
              (AUTO.DFA.SET/NFA/NODES PRODUCT p/idx n2)))
          (range (DFA.node-count dfa1)))))
     (range (DFA.node-count dfa2)))
    (define complement/state (AUTO.DFA.ADDNODE PRODUCT))
    (for-each
     (lambda (i2)
       (let ((x2 (AUTO.DFA.GET.TRANSITION/TABLE dfa2 i2)))
         (for-each
          (lambda (i1)
            (let ((x1 (AUTO.DFA.GET.TRANSITION/TABLE dfa1 i1)))
              (let ((k1 (hash-keys x1)))
                (define prod/from/state (product.index i2 i1))
                (set-for-each
                 k1 (lambda (in)
                      (define s1 (hash-ref x1 in))
                      (define s2 (hash-ref x2 in #f))
                      (define prod/to/state
                        (if s2
                            (product.index s2 s1)
                            complement/state))
                      (AUTO.DFA.SET.TRANSITION
                       PRODUCT prod/from/state in prod/to/state))))))
          (range (DFA.node-count dfa1)))))
     (range (DFA.node-count dfa2)))
    ;; (AUTO.DFA.DBG dfa1 start1)
    ;; (AUTO.DFA.DBG dfa2 start2)
    ;; (AUTO.DFA.DBG PRODUCT product/start/node)
    (return PRODUCT
            product/start/node 
            complement/state)))

(define dfa->nfa
  (lambda (dfa start return)
    (define new.nfa (AUTO.NFA 1000000))
    'todo))

(define dfa->nfa.reversed
  (lambda (dfa start return)
    (define rev (AUTO.NFA 1000000))
    (for-each 
     (lambda (dfa.node)
       (define new (AUTO.NFA.ADDNODE rev))
       (and (= new start)
            (AUTO.NFA.SETEXIT rev new)))
     (range (DFA.node-count dfa)))
    (for-each 
     (lambda (dfa.node)
       (hash-for-each
        (AUTO.DFA.GET.TRANSITION/TABLE dfa dfa.node)
        (lambda (in to)
          (AUTO.NFA.SET.TRANSITION rev to in dfa.node))))
     (range (DFA.node-count dfa))) 
    (define nfa.start (AUTO.NFA.ADDNODE rev))
    (for-each 
     (lambda (dfa.node)
       (and (AUTO.DFA.GET.EXIT dfa dfa.node)
            (AUTO.NFA.SET.EPSILON-TRANSITION rev nfa.start dfa.node)))
     (range (DFA.node-count rev)))
    (return rev nfa.start)))

(define make/test/dfa
  (lambda (return)
    "this is just for debugging"
    (define TEST ;;"(x([ab]*|u)[cd]y*)*##[12]?"
      `(COMPLEMENT "abc"))
    ((desugar TEST)
     (lambda (d)
       (mk/epsilon/nfa
        d
        (lambda (e.nfa start0 end0)
          (epsilon/nfa->dfa
           e.nfa start0 end0
           (lambda (dfa dfa.start)
             (dfa.myhill–nerode.minimize
              dfa dfa.start
              (lambda (dfa dfa.start)
                (return dfa dfa.start)))))))))))

(define dfa->langop
  (lambda (dfa dfa.start return)
    "we supposed a minimized dfa, otherwise it may stuck."
    ;; (__d ".dfa->langop")
    
    ;; (AUTO.DFA.DBG dfa dfa.start)
    
    (define dfasize     (DFA.node-count dfa))
    (define dfarange    (range dfasize))
    (define valid/nodes (make-vector dfasize #t))
    
    
    (define re/matrix
      (lambda ()
        (define input  (make-vector  dfasize #f))
        (define output (make-vector  dfasize #f))
        (define loop   (make-vector  dfasize #f))
        (define matrix (vector-map (lambda _ (make-vector dfasize #f))
                                   (list->vector dfarange)))
        (define XYget (lambda (x y) (vector-ref (vector-ref matrix x) y)))
        (define XYset (lambda (x y v) (vector-set! (vector-ref matrix x) y v)))
        (lambda (M x y v . w)
          (case M
            ((M) matrix)
            ((+)
             (if (= x y)
                 (vector-set! loop x #t)
                 (let ((in (vector-ref input y))
                       (out (vector-ref output x)))
                   (vector-set! input y (set-add (or in (set)) x))
                   (vector-set! output x (set-add (or out (set)) y))))
             (XYset x y v))
            ((-)
             (and (vector-ref input x)
                  (set-for-each
                   (vector-ref input x)
                   (lambda (from)
                     (let ((from->out (vector-ref output from)))
                       (and from->out
                            (vector-set! output from
                                         (set-remove from->out x))))
                     (XYset from x #f))))
             (and (vector-ref output x)
                  (set-for-each
                   (vector-ref output x)
                   (lambda (to)
                     (let ((in->to (vector-ref input to)))
                       (and in->to
                            (vector-set!
                             input to
                             (set-remove in->to x))))
                     (XYset x to #f))))
             (and (vector-ref loop x)
                  (XYset x x #f))
             (vector-set! input x #f)
             (vector-set! output x #f)
             (vector-set! loop x #f))
            ((GET)     (XYget x y))
            ((IN)      (and (vector-ref input x)
                            (set-map (vector-ref input x)
                                     (lambda (from)
                                       (list from (XYget from x))))))
            ((OUT)     (and (vector-ref output x)
                            (set-map (vector-ref output x)
                                     (lambda (to)
                                       (list to (XYget x to))))))
            ((LOOP)    (and (vector-ref loop x)
                            (XYget x x)))
            ((COPY)    (if (null? w)
                           (let ((new (re/matrix)))
                             (new 'COPY input output loop matrix)
                             new)
                           (begin
                             (set! input  (vector-copy x))
                             (set! output (vector-copy y))
                             (set! loop   (vector-copy v))
                             (set! matrix
                                   (vector-map vector-copy (car w))))))
            (else (error "matrix" M))))))
    
    (define initial/matrix (re/matrix))
    
    (define eliminate-state
      (lambda (s matrix)
        (define IN   (matrix 'IN s '_ '_))
        (define OUT  (matrix 'OUT s '_ '_))
        (define LOOP (matrix 'LOOP s '_ '_))
        (for-each
         (lambda (in)
           (define a1 (car in))
           (define re1 (cdr in))
           (for-each
            (lambda (out)
              (define a2 (car out))
              (define re2 (cdr out))
              (define prev/value (matrix 'GET a1 a2 '_))
              (define concat (append '(CONCAT) re1
                                     (if LOOP (list LOOP) '())
                                     re2))
              (define xx (if (= a1 a2)
                             `(REPEAT 0 INF ,concat)
                             concat))
              (matrix '+ a1 a2
                      (if prev/value
                          (list 'UNION prev/value xx)
                          xx)))
            (or OUT '())))
         (or IN '()))
        
        (matrix '- s '_ '_)))
    
    (define get-path
      (lambda (to/exit matrix)
        (for-each (lambda (s)
                    (or (= s dfa.start)
                        (= s to/exit)
                        (eliminate-state s matrix)))
                  dfarange)
        (define path/start->exit (matrix 'GET dfa.start to/exit   '_))
        (define path/exit->start (matrix 'GET to/exit dfa.start   '_))
        (define loop/exit        (matrix 'GET to/exit to/exit     '_))
        (define loop/start       (matrix 'GET dfa.start dfa.start '_)) 
        
        (define xx
          (if loop/exit
              `(CONCAT ,path/start->exit (REPEAT 0 INF ,loop/exit))
              path/start->exit))
        
        (if path/exit->start
            `(CONCAT (REPEAT 0 INF (UNION ,loop/start
                                          (CONCAT ,path/start->exit
                                                  ,loop/exit
                                                  ,path/exit->start)))
              ,xx)
            xx)))
    
    ;; compute initial regexp for each pair of nodes
    (for-each
     (lambda (node)
       (define xx (make-hash))
       (define edges (AUTO.DFA.GET.TRANSITION/TABLE dfa node))
       (hash-for-each
        edges
        (lambda (in to)
          (let ((prev (hash-ref xx to '())))
            (hash-set! xx to (cons in prev)))))
       (hash-for-each
        xx
        (lambda (to inputs)
          (define ii `(UNION . ,inputs))
          (initial/matrix '+ node to
                          (if (= node to)
                              `(REPEAT 0 INF ,ii)
                              ii)))))
     dfarange)
    
    ;; eliminate once forever the non-exit and non-start states
    (for-each
     (lambda (s)
       (or (AUTO.DFA.GET.EXIT dfa s)
           (= s dfa.start)
           (eliminate-state s initial/matrix)))
     dfarange)
    
    (define result '())
    
    (for-each (lambda (e/s)
                (and (AUTO.DFA.GET.EXIT dfa e/s)
                     (not (= e/s dfa.start))
                     (let ((path (get-path
                                  e/s (initial/matrix 'COPY'_'_'_))))
                       (and path (set! result (cons path result))))))
              dfarange)
    
    ;; maybe add (kleene start->start)
    (and (AUTO.DFA.GET.EXIT dfa dfa.start)
         (initial/matrix 'GET dfa.start dfa.start '_)
         (set! result (cons (initial/matrix 'GET dfa.start dfa.start '_)
                            result)))
    
    (return `(UNION . ,result))))

(define dfa.myhill–nerode.minimize
  (lambda (dfa start return)
    (__d ".dfa.minimize")
    
    ;; (AUTO.DFA.DBG dfa start) (exit 0)

    ;; (profiler 'min 'RESET)
    ;; (profiler 'min 'START)
    
    (define dfa.min (AUTO.DFA 1000000))
    (define dfa.min.start 0)
    (define blocks (union/find (DFA.node-count dfa)))
    (define blocks.unite (blocks 'UNITE))
    (define blocks.find (blocks 'FIND))
    (define blocks.group (blocks 'GROUP))

    ;; **************************************************

    (define exit-states
      (filter (lambda (x) x)
              (map (lambda (a b) (and a b))
                   (vector->list (AUTO.DFA.GET.EXITTABLE dfa))
                   (range (DFA.node-count dfa)))))

    ;; **************************************************

    (define dependable-pairs
      (list->vector
       (map (lambda (len) (make-vector (DFA.node-count dfa) '()))
            (range (DFA.node-count dfa)))))

    (define get/dependable
      (lambda (i j)
        (vector-ref (vector-ref dependable-pairs i) j)))

    (define add/dependable
      (lambda (i j new)
        (define xx (vector-ref dependable-pairs i))
        (vector-set! xx j (cons new (vector-ref xx j)))
        'ok))

    (define dbg/dependable
      (lambda ()
        "just for debug"
        (__d "--")
        (vector-map
         (lambda (x idx)
           (__d ":" idx "." x))
         dependable-pairs
         (list->vector (range (vector-length dependable-pairs))))))
    
    ;; **************************************************

    (define distinguishable-pairs
      (list->vector
       (map (lambda (len) (make-vector len #f))
            (range (DFA.node-count dfa)))))

    (define get/distinguishable
      (lambda (p)
        (define i (car p))
        (define j (cdr p))
        (vector-ref (vector-ref distinguishable-pairs i) j)))

    (define set/distinguishable
      (lambda (p)
        (define i (car p))
        (define j (cdr p))
        (vector-set!
         (vector-ref distinguishable-pairs i) j #t)))

    (define dbg/distinguishable
      (lambda ()
        "just for debug"
        (__d "--")
        (vector-map
         (lambda (x idx)
           (__d ":" idx "." x))
         distinguishable-pairs
         (list->vector (range (vector-length distinguishable-pairs))))))

    ;; **************************************************

    (define equivalent-pairs
      (apply append
             (map (lambda (i)
                    (map (lambda (j) (cons i j))
                         (range i)))
                  (range (DFA.node-count dfa)))))

    (define mk/group/idx
      (lambda ()
        (let ((node->group (blocks.group)))
          (let ((SS (list->set (vector->list node->group))))
            (let ((idx (make-hash
                        (map cons
                             (set->list SS)
                             (range (set-count SS))))))
              (set-for-each SS (lambda _ (AUTO.DFA.ADDNODE dfa.min)))
              (vector-map (lambda (g node)
                            (hash-ref idx g))
                          node->group
                          (list->vector (range (DFA.node-count dfa)))))))))

    ;; **************************************************

    (define set-initial-exit-inequivalences
      (lambda ()
        (for-each
         (lambda (e.state)
           (or (eq? (AUTO.DFA.GET.EXIT dfa (car e.state))
                    (AUTO.DFA.GET.EXIT dfa (cdr e.state)))
               (set/distinguishable e.state)))
         equivalent-pairs)))
    
    (define init/dependable
      (lambda ()
        (for-each
         (lambda (pair)
           (define a (car pair))
           (define b (cdr pair))
           ((lambda (s) (s s ALPHABET-SIZE))
            (lambda (s in)
              (or (negative? in)
                  (let ((to1 (AUTO.DFA.GET.TRANSITION dfa a in))
                        (to2 (AUTO.DFA.GET.TRANSITION dfa b in)))
                    (if (and (eq? #f to1) (eq? #f to2))
                        (s s (sub1 in))
                        (if (or (eq? #f to1) (eq? #f to2))
                            (set/distinguishable pair)
                            (and (add/dependable to1 to2 pair)
                                 (s s (sub1 in))))))))))
         equivalent-pairs)))
    
    (define transitive/closure
      (lambda (pairs* return)
        ((lambda (s) (s s pairs* return))
         (lambda (s border col)
           (if (null? border)
               (col '())
               (s s (cdr border)
                  (lambda (REST)
                    (let ((x (caar border))
                          (y (cdar border)))
                      ((lambda (u) (u u (get/dependable x y) col))
                       (lambda (u deps* col)
                         (if (null? deps*)
                             (col REST)
                             (if (get/distinguishable (car deps*))
                                 (u u (cdr deps*) col)
                                 (u u (cdr deps*)
                                    (lambda (rest)
                                      (set/distinguishable (car deps*))
                                      (col (cons (car deps*) rest))))))))))))))))
    
    ;; **************************************************
    
    (init/dependable)
    (set-initial-exit-inequivalences)
    
    ((lambda (s) (s s (filter get/distinguishable equivalent-pairs)))
     (lambda (s border/distinguishable)
       (transitive/closure border/distinguishable
                           (lambda (next/border)
                             (or (null? next/border)
                                 (s s next/border))))))
    
    (for-each (lambda (s)
                (or (get/distinguishable s)
                    (blocks.unite (car s) (cdr s))))
              equivalent-pairs)
    
    (let ((group/idx (mk/group/idx)))
      (for-each (lambda (from)
                  (define group/from (vector-ref group/idx from))

                  (AUTO.DFA.SET/NFA/NODES dfa.min group/from (set))

                  (and (AUTO.DFA.GET.EXIT dfa from)
                       (AUTO.DFA.SETEXIT dfa.min group/from))

                  (and (= from start)
                       (set! dfa.min.start group/from))
                  (for-each (lambda (input)
                              (define to (AUTO.DFA.GET.TRANSITION dfa from input))
                              (define group/to (vector-ref group/idx to))
                              (AUTO.DFA.SET.TRANSITION dfa.min group/from input group/to))
                            (hash-keys (AUTO.DFA.GET.TRANSITION/TABLE dfa from))))
                (range (DFA.node-count dfa))))
    
    ;; (AUTO.DFA.DBG dfa.min dfa.min.start)
    ;; (profiler 'min 'END)
    ;; (__d "~dfamin." (/ (apply + (profiler 'min 'GET)) 1000))    
    
    (return dfa.min dfa.min.start)))

