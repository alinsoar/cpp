;; -*- mode:scheme ; buffer-read-only:t -*-

(define nil?
  (lambda (a)
    (eq? 'NIL a)))

(define lex_evaluator_with_groups
  (lambda (STARTNODE EXIT-CODES SEND-START SEND-END TRANSITIONS TRIE)
    (lambda (STREAM INIT-POINTER)
      
      (define SLENGTH (vector-length STREAM))
      (define POINTER INIT-POINTER)
      (define TYPE 'NIL)
      (define LINE 1)
      (define COLUMN 0)
      
      (define next.
        (lambda (point state env-exit env-capt val splice L C ret/tok)
          (define STATE-TRANS (vector-ref TRANSITIONS state))
          (define INPUT       (vector-ref STREAM point))
          (define INPUT2      (and (< (add1 point) SLENGTH)
                                   (vector-ref STREAM (add1 point))))
          (define NEXT-STATE  (if (< INPUT 128)
                                  (vector-ref STATE-TRANS INPUT)
                                  'MULTI-CHAR))
          (define EXIT-CODE   (vector-ref EXIT-CODES  state))
          (define CAPTURE     (vector-ref SEND-START  state))
          (define SEND        (vector-ref SEND-END    state))

          ;; read tables
          (or (nil? EXIT-CODE)
              (set! env-exit
                    (lambda (_ endpos2 exit2)
                      (ret/tok point ((TRIE 'NEW!)) EXIT-CODE val
                               L C splice endpos2 exit2))))
          
          (or (nil? CAPTURE)
              (for-each (lambda (code)
                          (hash-set! env-capt code
                                     (lambda (ret-start-capture-point)
                                       (ret-start-capture-point point))))
                        CAPTURE))
          
          (or (nil? SEND)
              (for-each (lambda (code)
                          ((hash-ref env-capt code)
                           (lambda (capt-start-point)
                             (hash-set!
                              val
                              (cons capt-start-point code) point))))
                        SEND))

          ;; Body
          (cond ((false? NEXT-STATE)
                 (env-exit INPUT point EXIT-CODE))
                ((eq? 'MULTI-CHAR NEXT-STATE)
                 (set! COLUMN (add1 COLUMN))
                 (WARNING 'LEXER
                       "MULTI BYTE CHARS ignored" ";;;"
                       LINE "~" (sub1 COLUMN))
                 (next. (add1 point)
                        state
                        env-exit
                        env-capt
                        val
                        splice LINE (sub1 COLUMN)
                        ret/tok))
                ((and (= INPUT BS) (= INPUT2 NL))
                 (define L0 LINE)
                 (define C0 COLUMN)
                 (set! LINE (add1 LINE))
                 (set! COLUMN 0)
                 (next. (+ 2 point)
                        state
                        env-exit
                        env-capt
                        val
                        (cons (add1 point) splice) L0 C0
                        ret/tok))
                ((<= SLENGTH (add1 point))
                 
                 (define EXIT-CODE2 (vector-ref EXIT-CODES NEXT-STATE))
                 (define CAPTURE2   (vector-ref SEND-START NEXT-STATE))
                 (define SEND2      (vector-ref SEND-END   NEXT-STATE))
                 
                 (or (nil? EXIT-CODE2)
                     (set! env-exit
                           (lambda (_ endpos2 exit2)
                             (ret/tok (add1 point) ((TRIE 'NEW!)) EXIT-CODE2 val
                                      L C splice endpos2 exit2))))
                 
                 (or (nil? SEND2)
                     (for-each (lambda (code)
                                 ((hash-ref env-capt code)
                                  (lambda (start)
                                    (hash-set!
                                     val
                                     (cons start code)
                                     point))))
                               SEND2))
                 (env-exit '_ point EXIT-CODE))
                (else
                 ((TRIE 'INSERT-CHAR) INPUT)
                 (define L0 LINE)
                 (define C0 COLUMN)
                 (if (= INPUT NL)
                     (begin
                       (set! LINE (add1 LINE))
                       (set! COLUMN 0))
                     (set! COLUMN (add1 COLUMN)))
                 (next. (add1 point)
                        NEXT-STATE
                        env-exit
                        env-capt
                        val
                        splice L0 C0
                        ret/tok)))))
      
      (lambda (s f)
        (define L0 LINE)
        (define C0 COLUMN)
        (cond ((>= POINTER SLENGTH)
               (f (cons L0 C0)))
              (else
               (next. POINTER
                      STARTNODE
                      (lambda (INPUT _ __)
                        (PANIC "LEXER: not recognized input character --"
                               (~a "`"
                                   (if (> INPUT 32)
                                       (integer->char INPUT)
                                       INPUT)
                                   "`"
                                   " ; [" L0 ":" C0 "]; "
                                   POINTER
                                   ";")))
                      (make-hash)
                      (make-hash)
                      '() L0 C0
                      (lambda (end-point uniqueid type value L C S endpos2 exitcode2)
                        (define start-point POINTER)
                        (define captured/data
                          (hash-map
                           value
                           (lambda (k v)
                             (let ((start (car k))
                                   (end v)
                                   (code (cdr k)))
                               (list code
                                     (map (lambda (pos) (vector-ref STREAM pos))
                                          (range start end))
                                     start end)))))
                        (set! POINTER end-point)
                        (s type uniqueid captured/data start-point end-point
                           (cons L0 C0) (cons L C) S
                           (and (= (add1 endpos2) SLENGTH)
                                (nil? exitcode2)
                                (< (add1 start-point) SLENGTH)))))))))))

(define make-lexer-groups
  (lambda (grammar-file lex_trie)
    (load/grammar
     grammar-file
     (lambda (STARTNODE EXIT-CODES SEND-START SEND-END TRANSITIONS SYMS0 SS0 SE0)
       (lex_evaluator_with_groups STARTNODE
                                  EXIT-CODES SEND-START SEND-END
                                  TRANSITIONS
                                  lex_trie
                                  )))))

