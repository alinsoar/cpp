;; -*- mode:scheme ; buffer-read-only:t -*-

(define filter-out-nil
  (lambda (l)
    (filter (lambda (s) (not (eq? 'NIL s))) l)))

;;; SAVE

(define HEADER
  ";;; -- this file was automatically generated by bnf compiler ")

(define save-exit-code-table
  (lambda (EXIT-TABLE)
    (define exit-symbols
      (map string->symbol
           (sort (map symbol->string
                      (set->list (list->set (vector->list EXIT-TABLE))))
                 string<?)))
    (define indexes (make-hash
                     (map cons
                          exit-symbols
                          (range (length exit-symbols)))))
    (define codes
      (let ((states (make-hash)))
        (map (lambda (state)
               (let ((exit-code (vector-ref EXIT-TABLE state)))
                 (let ((v (hash-ref states exit-code '())))
                   (hash-set! states exit-code (cons state v)))))
             (range (vector-length EXIT-TABLE)))
        ;; (__e states)
        states))
    
    (display ";; exit code symbols") (newline)
    (write (filter-out-nil exit-symbols)) (newline)
    (map
     (lambda (s)
       (display "(")
       (display s)
       (for-each (lambda (c) (display " ") (write c))
                 (hash-ref codes s))
       (display ")")
       (newline))
     (filter-out-nil exit-symbols))))

(define save-start-and-end-positions-captured-symbols
  (lambda (start/table end/table)
    (define CAPTURED-SYMBOL-SETS
      (filter-out-nil
       (set->list
        (list->set
         (append start/table end/table)))))
    (define CAPTURED-SYMBOL-SETS-COUNT
     (length CAPTURED-SYMBOL-SETS))
    (define syms->indexes
      (make-hash
       (cons '(NIL . _)
             (map cons
                  CAPTURED-SYMBOL-SETS
                  (range CAPTURED-SYMBOL-SETS-COUNT)))))
    
    (define preprocess-table-data
      (lambda (table vect)
        (for-each (lambda (syms state)
                    (or (eq? syms 'NIL)
                        (let ((symsidx (hash-ref syms->indexes syms)))
                          (let ((v (vector-ref vect symsidx)))
                            (vector-set! vect symsidx
                                         (if (eq? 'empty v)
                                             (list state)
                                             (cons state v)))))))
                  table
                  (range (length table)))
        vect))
    
    (define start-table/syms->states
      (preprocess-table-data
       start/table
       (make-vector CAPTURED-SYMBOL-SETS-COUNT 'empty)))

    (define end-table/syms->states
      (preprocess-table-data
       end/table
       (make-vector CAPTURED-SYMBOL-SETS-COUNT 'empty)))
    
    (display ";;; number of captured groups of symbols") (newline)
    (display CAPTURED-SYMBOL-SETS-COUNT) (newline)
    
    (define allsyms (set->list (list->set (apply append CAPTURED-SYMBOL-SETS))))
    (define hashsyms (make-hash (map cons allsyms (range (length allsyms)))))
    
    (newline) (display ";;; captured symbols") (newline)
    (display "#(")
    (for-each
     (lambda (a i) (newline) (display a))
     allsyms
     (range (length allsyms)))
    (newline)
    (display ")") (newline) (newline)
    
    (display ";;; captured groups of symbols") (newline)
    (for-each
     (lambda (ss idx)
       (display (cons idx (map (lambda (s) (hash-ref hashsyms s)) ss)))
       (newline))
     CAPTURED-SYMBOL-SETS
     (range (length CAPTURED-SYMBOL-SETS)))
    (newline) (newline)
    
    (define write/table
      (lambda (tname syms->states)
        (display ";; ") (display tname) (display " table") (newline)
        (map
         (lambda (s)
           (write s) (newline))
         (map (lambda (symsidx states)
                (if (eq? 'empty states)
                    'NIL
                    (list symsidx states)))
              (range (vector-length syms->states))
              (vector->list syms->states)))
        (newline) (newline)))
    
    (write/table "ss" start-table/syms->states)
    (write/table "se" end-table/syms->states)))

(define save-transition-table
  (lambda (TRANSITION-TABLE)
    (define unique/states
      (filter-out-nil
       (set->list
        (list->set
         (vector->list TRANSITION-TABLE)))))
    
    (define search-unique/state/idx
      (lambda (state)
        (define iter
          (lambda (idx s)
            (if (equal? (car s) state)
                idx
                (iter (add1 idx) (cdr s)))))
        (if (eq? 'NIL state)
            'NIL
            (iter 0 unique/states))))
    
    (define unique/state/idx
      (vector-map search-unique/state/idx TRANSITION-TABLE))
    
    
    (display ";;; unique states") (newline)
    (display (length unique/states)) (newline)

    (define xx (list->vector (range ALPHABET-SIZE)))
    (for-each (lambda (unique/s i)
                (display ";;; unique state ") (display i) (newline)
                (display "(")
                (vector-map
                 (lambda (input next-state)
                   (and next-state
                        (display (cons input next-state))))
                 xx
                 unique/s)
                (display ")")
                (newline)
                (newline))
              unique/states
              (range (length unique/states)))
    
    (newline)
    
    (display ";;; unique state indexes") (newline)
    (display (vector->list unique/state/idx))
    (newline)))

(define save-automaton
  (lambda (FILE)
    (lambda (startnode TRANSITION-TABLE EXIT-TABLE SS-TABLE SE-TABLE)
      (__d "save compiled grammar to file" (~a #\` FILE #\`))
      (with-output-to-file FILE
        (lambda ()
          (display HEADER) (newline)
          (display ";;; start node") (newline)
          (write startnode) (newline)
          (display ";;; number of states") (newline)
          (write (add1 (counter/get 'DFA-NODE))) (newline)
          (save-exit-code-table EXIT-TABLE) (newline)(newline)
          (save-start-and-end-positions-captured-symbols SS-TABLE SE-TABLE)
          (save-transition-table TRANSITION-TABLE))
        #:mode   'binary
        #:exists 'truncate/replace)
      'OK)))

;;; LOAD

(define load/grammar/exit-codes
  (lambda (F state-count)
    (define EXIT-CODES (read F))
    (define exit-code-table (make-vector state-count 'NIL))
    (for-each
     (lambda (c)
       (let ((tab (read F)))
         (or (eq? c (car tab)) (error "load/grammar/exit-codes"))
         (for-each 
          (lambda (rank)
            (vector-set! exit-code-table rank c))
          (cdr tab))))
     EXIT-CODES)
    
    exit-code-table))

(define load/capture/start/end/tables
  (lambda (F total-states ret/tables)
    (define number-of-sets (read F))
    (define all-symbols (read F))
    
    (define SETS (make-vector number-of-sets 'NIL))
    (define SETS0 (make-vector number-of-sets 'NIL))
    (define SS/TABLE (make-vector total-states 'NIL))
    (define SE/TABLE (make-vector total-states 'NIL))
    (define SS/TABLE0 (make-vector total-states 'NIL))
    (define SE/TABLE0 (make-vector total-states 'NIL))
    
    (define iter
      (lambda (counter f)
        (if (zero? counter)
            'ok
            (begin
              (f (read F))
              (iter (sub1 counter)
                    f)))))
    
    (define load-table
      (lambda (table table0)
        (iter number-of-sets
              (lambda (data)
                (or (eq? 'NIL data)
                    (let ((start/capture/symbols
                           (vector-ref SETS (car data)))
                          (start/capture/symbols0
                           (vector-ref SETS0 (car data))))
                      (for-each
                       (lambda (rank)
                         (vector-set! table rank start/capture/symbols)
                         (vector-set! table0 rank start/capture/symbols0))
                       (cadr data))))))))
    
    (iter number-of-sets
          (lambda (data)
            (vector-set! SETS (car data)
                         (map (lambda (i) (vector-ref all-symbols i))
                              (cdr data)))
            (vector-set! SETS0 (car data) (cdr data))))

    (load-table SS/TABLE SS/TABLE0)
    (load-table SE/TABLE SE/TABLE0)
    
    (ret/tables SS/TABLE SE/TABLE all-symbols SS/TABLE0 SE/TABLE0)))

(define load/transition/table
  (lambda (F TRANSITIONS)
    (define unique/state/count (read F))
    (define unique/states (make-vector unique/state/count))
    
    (for-each
     (lambda (idx)
       (let ((s (read F))
             (v (make-vector ALPHABET-SIZE #f)))
         (vector-set! unique/states idx v)
         (for-each (lambda (i) (vector-set! v (car i) (cdr i)))
                   s)))
     (range unique/state/count))
    
    (let ((transitions (read F)))
      
      (map
       (lambda (i t)
         (vector-set! TRANSITIONS i (vector-ref unique/states t)))
       (range 1 (length transitions))
       (cdr transitions)))
    
    'done ))

(define load/grammar
  (lambda (file ret)
    (define STARTNODE   'NIL)
    (define TOTALNODES  'NIL)
    (define TRANSITIONS 'NIL)
    (define SEND-START  'NIL)
    (define SEND-END    'NIL)
    (define EXIT-CODES  'NIL)
    
    (or (file-exists? file)
        (begin
          (PANIC
           "should compile the lexical grammar using bnf compiler --" file ".")
          (exit 1)))
    
    (define F (open-input-file file #:mode 'binary))
    
    (set! STARTNODE   (read F))
    (set! TOTALNODES  (read F))
    (set! EXIT-CODES  (load/grammar/exit-codes F TOTALNODES))
    (load/capture/start/end/tables
     F TOTALNODES
     (lambda (ss se syms ss0 se0)
       (set! SEND-START ss)
       (set! SEND-END se)
       
       (set! TRANSITIONS (make-vector TOTALNODES 'NIL))
       
       (load/transition/table F TRANSITIONS)
       (ret STARTNODE EXIT-CODES SEND-START SEND-END TRANSITIONS syms ss0 se0)))))

;;; LEX grammar

(define lex-file-buffer
  (lambda (filename lexer ret)
    (if (file-exists? filename)
        (let ((STREAM (list->vector
                       (file->list filename read-byte #:mode 'text))))
          (ret (lexer STREAM 0) STREAM))
        (begin
          (PANIC "lex-file: cannot find file" filename)
          (exit 1)))))
