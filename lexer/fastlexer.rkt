;; -*- mode:scheme ; buffer-read-only:nil -*-

(define fastlex_evaluator
  (lambda (STARTNODE EXIT-CODES TRANSITIONS)
    (lambda (STREAM INIT-POINTER)
      
      (define SLENGTH (vector-length STREAM))
      (define POINTER INIT-POINTER)
      (define TYPE 'NIL)
      (define LINE 1)
      (define COLUMN 0)
      
      (define next.
        (lambda (point state env-exit splice L C acc ret/tok)
          (define STATE-TRANS (vector-ref TRANSITIONS state))
          (define INPUT       (vector-ref STREAM point))
          (define INPUT2      (and (< (add1 point) SLENGTH)
                                   (vector-ref STREAM (add1 point))))
          (define NEXT-STATE  (vector-ref STATE-TRANS INPUT))
          (define EXIT-CODE   (vector-ref EXIT-CODES  state))
          
          (or (eq? 'NIL EXIT-CODE)
              (set! env-exit
                    (lambda ()
                      (ret/tok point EXIT-CODE L C splice acc))))
          
          (cond ((and (= INPUT BS) (= INPUT2 NL))
                 ;; splice line has the greatest priority
                 (next. (+ 2 point)
                        state
                        env-exit
                        (cons point splice) (add1 L) C
                        acc
                        ret/tok))
                ((false? NEXT-STATE)
                 ;; end of token
                 (env-exit))
                ((<= SLENGTH (add1 point))
                 ;; end of stream
                 (define EXIT-CODE2
                   (vector-ref EXIT-CODES NEXT-STATE))
                 
                 (or (eq? 'NIL EXIT-CODE2)
                     (set! env-exit
                           (lambda ()
                             (ret/tok (add1 point) EXIT-CODE2
                                      L C splice acc))))
                 
                 (env-exit))
                (else
                 ;; try next input
                 (next. (add1 point)
                        NEXT-STATE
                        env-exit
                        splice
                        (if (= INPUT NL) (add1 L) L)
                        (if (= INPUT NL) 0 (add1 C))
                        (cons INPUT acc)
                        ret/tok)))))
      
      (lambda (s f)
        (cond ((>= POINTER SLENGTH)
               (f LINE COLUMN))
              (else
               (next. POINTER
                      STARTNODE
                      (lambda ()
                        "no grammar symbol matches"
                        (define INPUT (vector-ref STREAM POINTER))
                        (PANIC "LEXER: not recognized input character --"
                               (~a "`"
                                   (if (> INPUT 32)
                                       (integer->char INPUT)
                                       INPUT)
                                   "`"
                                   " ; [" LINE ":" COLUMN "]; "
                                   POINTER
                                   ";")))
                      '()
                      LINE
                      COLUMN
                      '()
                      (lambda (end-point type coordinate.L coordinate.C S acc)
                        (let ((coordinate.L0 LINE)
                              (coordinate.C0 COLUMN)
                              (start-point POINTER)
                              (dbg/splice (and (cons? S) (map (lambda (x) (- x POINTER)) S))))
                          (set! LINE coordinate.L)
                          (set! COLUMN coordinate.C)
                          (set! POINTER end-point)
                          (s type
                             coordinate.L0 coordinate.C0
                             coordinate.L coordinate.C
                             dbg/splice acc))))))))))

(define make-fastlexer
  (lambda (grammar-file)
    (load/grammar
     grammar-file
     (lambda (STARTNODE EXIT-CODES SEND-START SEND-END TRANSITIONS SYMS0 SS0 SE0)
       (or (zero? (vector-length SYMS0))
           (begin
             (PANIC "fast lexer does not accept groups.")
             (exit 1)))
       (fastlex_evaluator STARTNODE EXIT-CODES TRANSITIONS)))))
