;; -*- mode:scheme ; buffer-read-only:nil -*-

(define AUTO.NFA.DBG
  (lambda (nfa)
    (__d "==")
    (vector-map
     (lambda (v idx)
       (__d "++" idx
            (if (AUTO.NFA.GET.EXIT nfa idx)
                "***"
                ""))
       (hash-for-each
        v
        (lambda (a b)
          (define to
            (foldr (lambda (a b) (~a a " " b)) " "
                   (set->list b)))
          (__d idx
               "." (__fcol 10 'right " " (if (eq? a ':epsilon) "" a))
               "=>" to))))
     (AUTO.NFA.GET.TRANSITION nfa)
     (list->vector (range (NFA.node-count nfa))))
    'nfa/dbg))

(define AUTO.DFA.DBG
  (lambda (dfa start)
    (__d "--" (DFA.node-count dfa))
    (vector-map
     (lambda (v idx)
       (__p (__fcol 5 'left " " idx)
            (if (AUTO.DFA.GET.EXIT dfa idx)
                " $$$ {  "
                " ~~~ {  "))
       (if (and (set? (AUTO.DFA.GET.NFANODES dfa idx))
                (< (set-count (AUTO.DFA.GET.NFANODES dfa idx)) 20))
           (set-for-each (AUTO.DFA.GET.NFANODES dfa idx)
                         (lambda (x) (__p x " ")))
           (__p "... "))
       (__d "} " (if (= idx start) ">>>" ""))
       (define sorted/edges
         (sort (hash->list v) 
               (lambda (a b)
                 (< (car a) (car b)))))
       (for-each
        (lambda (a b)
            (__d (__fcol 12 'right " "
                         (if (and (> a 32) (< a 127))
                             (integer->char a)
                             a))
                 "=>" b))
        (map car sorted/edges)
        (map cdr sorted/edges)))
     (AUTO.DFA.GET.ALLTRANSITIONS dfa)
     (list->vector (range (DFA.node-count dfa))))
    'dfa/dbg))

