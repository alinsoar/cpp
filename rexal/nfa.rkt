;; -*- mode:scheme ; buffer-read-only:t -*-

;;; Given some desugared operator language input, makes the epsilon-nfa

(define dlang~concat
  (lambda (rand*)
    (lambda (nfa return)
      ((lambda (s) ((d/lang->epsilon/nfa (car rand*))
               nfa (lambda (START end)
                     (return START (s s (cdr rand*) end)))))
       (lambda (s r* last/node)
         (if (null? r*)
             last/node
             ((d/lang->epsilon/nfa (car r*))
              nfa (lambda (start* end*)
                    (AUTO.NFA.SET.EPSILON-TRANSITION nfa last/node start*)
                    (s s (cdr r*) end*)))))))))

(define dlang~union
  (lambda (rand*)
    (lambda (nfa return)
      (define START (AUTO.NFA.ADDNODE nfa))
      (define END (AUTO.NFA.ADDNODE nfa))
      (and (null? rand*)
           (AUTO.NFA.SET.EPSILON-TRANSITION nfa START END))
      ((lambda (s) (s s rand*))
       (lambda (s r*)
         (if (null? r*)
             (return START END)
             ((d/lang->epsilon/nfa (car r*))
              nfa (lambda (start* end*)
                    (AUTO.NFA.SET.EPSILON-TRANSITION nfa START start*)
                    (AUTO.NFA.SET.EPSILON-TRANSITION nfa end* END)
                    (s s (cdr r*))))))))))

(define dlang~kleene
  (lambda (rand)
    (lambda (nfa return)
      (define START (AUTO.NFA.ADDNODE nfa))
      (define END (AUTO.NFA.ADDNODE nfa))
      (AUTO.NFA.SET.EPSILON-TRANSITION nfa END START)
      (AUTO.NFA.SET.EPSILON-TRANSITION nfa START END)
      ((d/lang->epsilon/nfa rand)
       nfa (lambda (start* end*)
             (AUTO.NFA.SET.EPSILON-TRANSITION nfa START start*)
             (AUTO.NFA.SET.EPSILON-TRANSITION nfa end* END)
             (return START END))))))

(define dlang~bol
  (lambda (rand)
    (lambda (nfa return)
      ((d/lang->epsilon/nfa rand)
       nfa (lambda (start end)
             (AUTO.NFA.SETBOL nfa start)
             (return start end))))))

(define dlang~atomic
  (lambda (in)
    (lambda (nfa return)
      (define START (AUTO.NFA.ADDNODE nfa))
      (define END (AUTO.NFA.ADDNODE nfa))
      (AUTO.NFA.SET.TRANSITION nfa START in END)
      (return START END))))

(define d/lang->epsilon/nfa
  (lambda (in)
    (cond ((integer?   in) (dlang~atomic       in))
          ((OP.UNION?  in) (dlang~union   (cdr in)))
          ((OP.CONCAT? in) (dlang~concat  (cdr in)))
          ((OP.KLEENE? in) (dlang~kleene (cadr in)))
          ((OP.BOL?    in) (dlang~bol    (cadr in)))
          (else (error "nfa:" in)))))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define nfa/transitive/closure
  (lambda (enfa return)
    (define count (NFA.node-count enfa))
    (define epsilon-edges (AUTO.NFA.GET.EPSILON-EDGES enfa))
    
    (define eclose
      (lambda (border visited)
        ((lambda (next)
           (define godel (nodes->godel.number next))
           (if (set-empty? next)
               border
               (if (set-member? visited godel)
                   border
                   (set-union border
                              (eclose next
                                      (set-union (set godel)
                                                 visited))))))
         (set-subtract
          (apply set-union
                 (set-map border
                          (lambda (x)
                            (vector-ref epsilon-edges x))))
          border))))
    
    (return
     (vector-map (lambda (idx)
                   (define initial/closure (set idx))
                   (eclose initial/closure
                           (set (nodes->godel.number initial/closure))))
                 (list->vector (range count))))))

(define nodes->godel.number
  (lambda (nodes)
    (apply *
           (map
            (lambda (x) (vector-ref primes x))
            (set-map nodes (lambda (x) x))))))

(define mk/epsilon/nfa
  (lambda (langop/input return)
    (let ((enfa (AUTO.NFA 1000000)))
      ((d/lang->epsilon/nfa langop/input)
       enfa
       (lambda (start end)
         (return enfa start end))))))

(define epsilon/nfa->dfa/warning #f)
(define epsilon/nfa->dfa
  (lambda (enfa start end return)
    ;; (__d ".epsilon/nfa->dfa")
    (define eclosure (nfa/transitive/closure enfa (lambda (eclo) eclo)))
    (define enfa/edges (AUTO.NFA.GET.EDGES enfa))
    (define enfa.exit.nodes (vector-ref eclosure end))
    (define DFA (AUTO.DFA 1000000))
    (define dfa/states/hashes (make-hash))
    
    (define add/dfa/state
      (lambda (nodes new old)
        
        ;; (profiler 'G 'START)
        (define godel (nodes->godel.number nodes))
        ;; (profiler 'G 'END)
        (if (hash-has-key? dfa/states/hashes godel)
            (old (hash-ref dfa/states/hashes godel))
            (let ((new/state (AUTO.DFA.ADDNODE DFA))
                  (xx #f))
              (AUTO.DFA.SET/NFA/NODES DFA new/state nodes)
              (set-for-each nodes (lambda (x)
                                    (and (set-member? enfa.exit.nodes x)
                                         (set! xx #t))))
              (and xx (AUTO.DFA.SETEXIT DFA new/state))
              (hash-set! dfa/states/hashes godel new/state)
              (new new/state)))))
    
    (define all/nodes/inputs
      (lambda (nodes)
        (apply append
               (set-map nodes
                        (lambda (x)
                          (hash-keys
                           (vector-ref enfa/edges x)))))))
    
    (define make-dfa-state
      (let ((vv (make-vector (NFA.node-count enfa) #f))
            (new 'nil) )
        (lambda (nodes input return)
          (vector-fill! vv #f)
          (set! new '())
          (set-for-each
           nodes (lambda (x)
                   (let ((to (hash-ref (vector-ref enfa/edges x) input #f)))
                     (and to
                          (set-for-each
                           to (lambda (a)
                                (or (vector-ref vv a)
                                    (begin
                                      (set! new (cons a new))
                                      (vector-set! vv a #t)))))))))
          (return (apply set-union
                         (map (lambda (x)
                                (vector-ref eclosure x))
                              new))))))
    
    (define make/subset/construction
      (lambda (nodes dfa/state)
        (let ((all/inputs (all/nodes/inputs nodes)))
          (for-each
           (lambda (input)
             (make-dfa-state
              nodes input
              (lambda (enclosed/nodes)
                (add/dfa/state enclosed/nodes
                               (lambda (new/dfa/state)
                                 (make/subset/construction
                                  enclosed/nodes new/dfa/state)
                                 (AUTO.DFA.SET.TRANSITION
                                  DFA dfa/state input new/dfa/state)
                                 nodes)
                               (lambda (new/dfa/state)
                                 (AUTO.DFA.SET.TRANSITION 
                                  DFA dfa/state input new/dfa/state)
                                 nodes)))))
           all/inputs))))
    
    ;; (AUTO.NFA.DBG enfa)
    (or epsilon/nfa->dfa/warning
        (WARNING "nfa->dfa may take some time"))
    (set! epsilon/nfa->dfa/warning #t)
    (profiler 'powerset/construction 'RESET)
    (profiler 'powerset/construction 'START)
    (let ((start.nodes (vector-ref eclosure start)))
      (add/dfa/state start.nodes
                     (lambda (initial/dfa/state)
                       (make/subset/construction start.nodes initial/dfa/state)
                       (profiler 'powerset/construction 'END)
                       ;; (__d (apply + (profiler 'A 'GET)))
                       ;; (__d (apply + (profiler 'G 'GET)))
                       ;; (__d (length (profiler 'G 'GET)))
                       ;; (__d (/ (apply + (profiler 'powerset/construction 'GET)) 1000))
                       ;; (AUTO.DFA.DBG DFA initial/dfa/state)
                       (return DFA initial/dfa/state))
                     (lambda () (error 'start-state-closure-is-new))))))

(define epsilon/nfa->dfa/NEW
  (lambda (enfa start end return)
    (define eclosure (nfa/transitive/closure enfa (lambda (eclo) eclo)))
    (define enfa/edges (AUTO.NFA.GET.EDGES enfa))
    (define enfa.exit.nodes (vector-ref eclosure end))
    (define DFA (AUTO.DFA 1000000))
    (define dfa/states/hashes (make-hash))
    
    (define add/dfa/state
      (lambda (nodes new old)
        
        ;; (profiler 'G 'START)
        (define godel (nodes->godel.number nodes))
        ;; (profiler 'G 'END)
        (if (hash-has-key? dfa/states/hashes godel)
            (old (hash-ref dfa/states/hashes godel))
            (let ((new/state (AUTO.DFA.ADDNODE DFA))
                  (xx #f))
              (AUTO.DFA.SET/NFA/NODES DFA new/state nodes)
              (set-for-each nodes (lambda (x)
                                    (and (set-member? enfa.exit.nodes x)
                                         (set! xx #t))))
              (and xx (AUTO.DFA.SETEXIT DFA new/state))
              (hash-set! dfa/states/hashes godel new/state)
              (new new/state)))))
    
    (define all/nodes/inputs
      (lambda (nodes)
        (apply append
               (set-map nodes
                        (lambda (x)
                          (hash-keys
                           (vector-ref enfa/edges x)))))))
    
    (define make-dfa-state
      (let ((vv (make-vector (NFA.node-count enfa) #f))
            (new 'nil) )
        (lambda (nodes input return)
          (vector-fill! vv #f)
          (set! new '())
          (set-for-each
           nodes (lambda (x)
                   (let ((to (hash-ref (vector-ref enfa/edges x) input #f)))
                     (and to
                          (set-for-each
                           to (lambda (a)
                                (or (vector-ref vv a)
                                    (begin
                                      (set! new (cons a new))
                                      (vector-set! vv a #t)))))))))
          (return (apply set-union
                         (map (lambda (x)
                                (vector-ref eclosure x))
                              new))))))
    
    (define make/subset/construction
      (lambda (nodes dfa/state)
        (let ((all/inputs (all/nodes/inputs nodes)))
          (for-each
           (lambda (input)
             (make-dfa-state
              nodes input
              (lambda (enclosed/nodes)
                (add/dfa/state enclosed/nodes
                               (lambda (new/dfa/state)
                                 (make/subset/construction
                                  enclosed/nodes new/dfa/state)
                                 (AUTO.DFA.SET.TRANSITION
                                  DFA dfa/state input new/dfa/state)
                                 nodes)
                               (lambda (new/dfa/state)
                                 (AUTO.DFA.SET.TRANSITION 
                                  DFA dfa/state input new/dfa/state)
                                 nodes)))))
           all/inputs))))
    
    ;; (AUTO.NFA.DBG enfa)
    (or epsilon/nfa->dfa/warning
        (WARNING "nfa->dfa may take some time"))
    (set! epsilon/nfa->dfa/warning #t)
    (profiler 'powerset/construction 'RESET)
    (profiler 'powerset/construction 'START)
    (let ((start.nodes (vector-ref eclosure start)))
      (add/dfa/state start.nodes
                     (lambda (initial/dfa/state)
                       (make/subset/construction start.nodes initial/dfa/state)
                       (profiler 'powerset/construction 'END)
                       ;; (__d (apply + (profiler 'A 'GET)))
                       ;; (__d (apply + (profiler 'G 'GET)))
                       ;; (__d (length (profiler 'G 'GET)))
                       ;; (__d (/ (apply + (profiler 'powerset/construction 'GET)) 1000))
                       ;; (AUTO.DFA.DBG DFA initial/dfa/state)
                       (return DFA initial/dfa/state))
                     (lambda () (error 'start-state-closure-is-new))))))

(define epsilon/nfa->dfa/DATED
  (lambda (enfa start end return)
    'DATED
    ;; (define eclosure (nfa/transitive/closure enfa (lambda (eclo) eclo)))
    ;; (define enfa/edges (AUTO.NFA.GET.EDGES enfa))
    ;; (define enfa.exit.nodes (vector-ref eclosure end))
    ;; (define DFA (AUTO.DFA 1000))
    ;; (define dfa/states/hashes (make-hash))
    
    ;; (define add/dfa/state
    ;;   (lambda (nodes new old)
    ;;     (define godel (nodes->godel.number nodes))
    ;;     (if (hash-has-key? dfa/states/hashes godel)
    ;;         (old (hash-ref dfa/states/hashes godel))
    ;;         (let ((new/state (AUTO.DFA.ADDNODE DFA)))
    ;;           (AUTO.DFA.SET/NFA/NODES DFA new/state nodes)
    ;;           (or (set-empty? (set-intersect nodes enfa.exit.nodes))
    ;;               (AUTO.DFA.SETEXIT DFA new/state))
    ;;           (hash-set! dfa/states/hashes godel new/state)
    ;;           (new new/state)))))
    
    ;; (define all/nodes/inputs
    ;;   (lambda (nodes)
    ;;     (apply append
    ;;            (set-map nodes
    ;;                     (lambda (x)
    ;;                       (hash-keys
    ;;                        (vector-ref enfa/edges x)))))))
    
    ;; (define make-dfa-state
    ;;   (lambda (nodes input)
    ;;     (let* ((new/nodes__
    ;;             (apply set-union
    ;;                    (set-map nodes
    ;;                             (lambda (x)
    ;;                               (hash-ref
    ;;                                (vector-ref enfa/edges x)
    ;;                                input
    ;;                                (set))))))
    ;;            (enclosed/nodes
    ;;             (apply set-union
    ;;                    (set-map new/nodes__
    ;;                             (lambda (x)
    ;;                               (vector-ref eclosure x))))))
    ;;       (add/dfa/state enclosed/nodes
    ;;                      (lambda (dfa/state)
    ;;                        (make/subset/construction
    ;;                         enclosed/nodes dfa/state)
    ;;                        dfa/state)
    ;;                      (lambda (dfa/state)
    ;;                        dfa/state)))))
    
    ;; (define make/subset/construction
    ;;   (lambda (nodes dfa/state)
    ;;     (let ((all/inputs (all/nodes/inputs nodes)))
    ;;       (for-each
    ;;        (lambda (input)
    ;;          (let ((new/dfa/state (make-dfa-state nodes input)))
    ;;            (AUTO.DFA.SET.TRANSITION
    ;;             DFA dfa/state input new/dfa/state)
    ;;            nodes))
    ;;        all/inputs))))
    
    ;; (__d "!!" start)
    ;; (AUTO.NFA.DBG enfa)
    ;; (let ((start.nodes (vector-ref eclosure start)))
    ;;   (add/dfa/state start.nodes
    ;;                  (lambda (initial/dfa/state)
    ;;                    (make/subset/construction
    ;;                     start.nodes initial/dfa/state)
    ;;                    (return DFA initial/dfa/state))
    ;;                  (lambda () (error 'start-state-closure-is-new))))
    ))
