;; -*- mode:scheme ; buffer-read-only:nil -*-

(define lex_trie
  (lambda ()
    (define ROOT (make-hash '((PARENT . NIL))))
    (define ROOT-NODEID 0)
    (define NODE-TABLE (make-hasheqv `((,ROOT-NODEID . ,ROOT))))
    (define CURRENT-NODE ROOT)
    (define CURRENT-NODEID ROOT-NODEID)
    (define nodes-count 1)
     ;; ---- ;; STATS
    ;; (define s:height 0)
    ;; (define s:height-count (make-hash))
    
    (define ++ch
      (lambda (ch at-node)
        (hash-ref
         at-node
         ch
         (lambda ()
           (let ((new-node
                  (make-hash
                   `((PARENT . ,CURRENT-NODEID)
                     (CH . ,ch))))
                 (new-node-id nodes-count))
             (hash-set! NODE-TABLE new-node-id new-node)
             (hash-set! CURRENT-NODE ch new-node-id)
             (set! nodes-count (add1 nodes-count))
             new-node-id)))))

    (define insert-char
      (lambda (ch)
        ;; (__d "-- insert" "[" (integer->char ch) "]" ch
        ;;      ";" "current node id" CURRENT-NODEID
        ;;      ;; "; node" CURRENT-NODE
        ;;      "; node count" nodes-count)
        (set! CURRENT-NODEID (++ch ch CURRENT-NODE))
        (set! CURRENT-NODE (hash-ref NODE-TABLE CURRENT-NODEID))
        ;; STATS
        ;; (set! s:height (add1 s:height))
        'ok))

    (define insert-char-at
      (lambda (ch at-node-id)
        (define at-node (hash-ref NODE-TABLE at-node-id))
        (++ch ch at-node)))

    (define get
      (lambda ()
        CURRENT-NODEID))
    
    (define MEMO (make-hash))
    
    (define fold-left
      (lambda (id combine unit final)
        (define iter
          (lambda (node acc)
            (let ((parent (hash-ref node 'PARENT)))
              (if (eq? 'NIL parent)
                  (let ((value (final acc)))
                    (hash-set! MEMO id value)
                    ;; (__d "$$" value)
                    value)
                  (iter (hash-ref NODE-TABLE parent)
                        (combine (hash-ref node 'CH)
                                 acc))))))
        (hash-ref
         MEMO
         id
         (lambda ()
           (iter (hash-ref NODE-TABLE id) unit)))))

    (define fold-right
      (lambda (id)
        'todo
        
        ))

    (define new!
      (lambda ()
        (let ((id CURRENT-NODEID))
          ;; (__d ":" (dbg id) ";" id)
          (set! CURRENT-NODEID ROOT-NODEID)
          (set! CURRENT-NODE ROOT)
          ;; ---- ;; STATS
          ;; (and (> s:height 20) (__d ":" (fold-left id cons '() (lambda (a) a))))
          ;; (hash-set! s:height-count s:height
          ;;            (add1 (hash-ref s:height-count s:height 0)))
          ;; (set! s:height 0)
          id)))
    
    (define dbg
      (lambda (id)
        (fold-left id
                   (lambda (a r)
                     (cons (integer->char a) r))
                   '()
                   list->string)))
    
    (define stats
      (lambda ()
        (__d "trie statistics")
        '(for-each
         (lambda (a)
           (__d "HEIHT" (car a) "COUNT" (cdr a)))
           
         (sort
          (hash-map
           s:height-count
           (lambda (HEIGHT COUNT) (cons HEIGHT COUNT)))
          (lambda (a b) (> (cdr a) (cdr b)))))))
    
    (lambda (m)
      (case m
        ('INSERT-CHAR      insert-char)
        ('INSERT-CHAR-AT   insert-char-at)
        ('GET              get)
        ('FOLD/LEFT        fold-left)
        ('FOLD/RIGHT       fold-right)
        ('DBG              dbg)
        ('NEW!             new!)
        ('STATS            stats)))))



