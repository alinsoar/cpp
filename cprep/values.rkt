;; -*- mode:scheme ; buffer-read-only:nil -*-

(define mkxtrie
  (lambda ()
    ;; (define stats:height 0)

    (define input-size-alphabet 256)

    (define memoize-accum (make-hasheq))
    (define (memo-acc-set! id value) (hash-set! memoize-accum id value))
    (define (memo-acc-ref id) (hash-ref memoize-accum id #f))

    (define VECTOR-BLOCK-SIZE (expt 2 16))

    (define ROOT-NODEIDX 0)

    (define TRANSITION-TABLE (make-vector input-size-alphabet 'nil))
    (for-each (lambda (i)
                (vector-set! TRANSITION-TABLE i (make-hasheq)))
              (range input-size-alphabet))
    (define PARENT-NODE-TABLE (make-vector VECTOR-BLOCK-SIZE 'nil))
    (define INPUT-TABLE       (make-vector VECTOR-BLOCK-SIZE 'nil))

    (define (GROW-PARENT-NODE-TABLE)
      (set! PARENT-NODE-TABLE
            (vector-append PARENT-NODE-TABLE
                           (make-vector VECTOR-BLOCK-SIZE 'nil))))

    (define (GROW-INPUT-TABLE)
      (set! INPUT-TABLE
            (vector-append INPUT-TABLE
                           (make-vector VECTOR-BLOCK-SIZE 'nil))))

    (define ($SET-PARENT-NODE-TABLE! nodeidx parentidx)
      (and (>= nodeidx (vector-length PARENT-NODE-TABLE))
           (GROW-PARENT-NODE-TABLE))
      (vector-set! PARENT-NODE-TABLE nodeidx parentidx))

    (define ($SET-INPUT-TABLE! nodeidx parentidx)
      (and (>= nodeidx (vector-length INPUT-TABLE))
           (GROW-INPUT-TABLE))
      (vector-set! INPUT-TABLE nodeidx parentidx))

    ($SET-PARENT-NODE-TABLE! ROOT-NODEIDX 'NIL)
    ($SET-INPUT-TABLE! ROOT-NODEIDX 'NIL)

    (define CURRENT-NODEIDX ROOT-NODEIDX)
    (define nodes-count 1)

    (define ($GETCH nodeidx) (vector-ref INPUT-TABLE nodeidx))
    (define ($PARENTIDX nodeidx) (vector-ref PARENT-NODE-TABLE nodeidx))

    (define ($REF-CURRENT-NODE-CH ch fail)
      (hash-ref (vector-ref TRANSITION-TABLE ch)
                CURRENT-NODEIDX fail))

    (define ($SET-CURRENT-NODE-CH! ch nodeidx)
      (hash-set! (vector-ref TRANSITION-TABLE ch)
                 CURRENT-NODEIDX nodeidx))

    (define ($SET-CURRENT-NODEIDX idx) (set! CURRENT-NODEIDX idx))

    (define ++ch
      (lambda (ch)
        ;; (set! stats:height (add1 stats:height))
        ($REF-CURRENT-NODE-CH
         ch
         (lambda ()
           (let ((new-node-idx nodes-count))
             ($SET-CURRENT-NODE-CH! ch new-node-idx)
             ($SET-PARENT-NODE-TABLE! new-node-idx CURRENT-NODEIDX)
             ($SET-INPUT-TABLE! new-node-idx ch)
             (set! nodes-count (add1 nodes-count))
             new-node-idx)))))

    (define insert-char!
      (lambda (ch)
        ($SET-CURRENT-NODEIDX (++ch ch))
        ;; (__d stats:height CURRENT-NODEID)
        ;; CURRENT-NODEIDX
        ))

    (define new!
      (lambda ()
        (let ((id CURRENT-NODEIDX))
          (set! CURRENT-NODEIDX ROOT-NODEIDX)
          ;; ---- STATS
          ;; (set! stats:height 0)
          id)))

    (define get.idx
      (lambda ()
        CURRENT-NODEIDX))

    (define insert-string!
      (lambda (word)
        (new!)
        (for-each insert-char!
                  (map char->integer
                       (string->list word)))
        (get.idx)))

    (define accumulator
      (lambda (id ret)
        "fold left and right"
        (define iter
          (lambda (nodeIDX acc col)
            (let ((parentIDX ($PARENTIDX nodeIDX)))
              (if (eq? 'NIL parentIDX)
                  (col acc '())
                  (let ((CH ($GETCH nodeIDX)))
                    (iter parentIDX (cons CH acc)
                          (lambda (acc/l acc/r)
                            (col acc/l (cons CH acc/r)))))))))
        (cond ((memo-acc-ref id) => (lambda (v) (ret (car v) (cdr v))))
              (else
               (iter id '()
                     (lambda (acc/l acc/r)
                       (memo-acc-set! id (cons acc/l acc/r))
                       (ret acc/l acc/r)))))))

    (lambda (m)
      (case m
        ('ROOT-NODEIDX          ROOT-NODEIDX)
        ('new!                  new!)
        ('insert-char!          insert-char!)
        ('insert-string!        insert-string!)
        ('current-nodeidx       get.idx)
        ('accum                 accumulator)
        ('count/stats           (cons nodes-count
                                      (hash-count memoize-accum)))
        (else (error "xtrie" m))))))

(define preprocessor-trie      (mkxtrie))
(define pptrie.insert-string   (preprocessor-trie 'insert-string!))
(define pptrie.current-nodeidx (preprocessor-trie 'current-nodeidx))
(define pptrie.accumulator     (preprocessor-trie 'accum))

(define c_lex_trie_values
  (lambda ()

    (define pptrie preprocessor-trie)

    (define MEMO (make-hasheq))
    (define (MEMO-SET! id value) (hash-set! MEMO id value) value)
    (define (MEMO-REF id fail) (hash-ref MEMO id fail))

    (define code->integer-suffix
      (let ((L 2) (U 3))
        (let ((suffixes
               (make-hasheq
                `((,1            . NONE)
                  (,L            . L)
                  (,U            . U)
                  (,(* L L)      . LL)
                  (,(* L U)      . UL)
                  (,(* L L U)    . ULL)
                  ))))
          (lambda (s) (hash-ref suffixes s)))))

    (define integer-suffix->code
      (let ((L 76) (u 117) (l 108) (U 85))
        (let ((codes (make-hasheq `((,L . 2) (,l . 2) (,u . 3) (,U . 3)))))
          (lambda (s) (hash-ref codes s #f)))))

    (define octal-digit-value
      (let ((oct.digits
             (make-hash
              '((48 . 0)  (49 . 1)  (50 . 2)   (51 . 3)   (52 . 4)
                (53 . 5)  (54 . 6)  (55 . 7)))))
        (lambda (n)
          (hash-ref oct.digits n #f))))

    (define digit-value
      (let ((digits
             (make-hash
              '((48 . 0)  (49 . 1)  (50 . 2)   (51 . 3)   (52 . 4)
                (53 . 5)  (54 . 6)  (55 . 7)   (56 . 8)   (57 . 9)
                (97 . 10) (98 . 11) (99 . 12) (100 . 13) (101 . 14) (102 . 15)
                (65 . 10) (66 . 11) (67 . 12)  (68 . 13)  (69 . 14)  (70 . 15)))))
        (lambda (n)
          ;;(lambda () (error "unknown digit --" n))
          (hash-ref digits n #f))))

    (define integer-value
      (lambda (BASE prefix-len)
        (lambda (id return)
          (define iter
            (lambda (ACC ret0)
              (if (null? ACC)
                  (ret0 '0 '1 '0)
                  (let ((CH (car ACC)))
                    (iter (cdr ACC)
                          (lambda (val suffix idx)
                            (let ((d (digit-value CH)))
                              (if (< idx prefix-len)
                                  (ret0 val
                                        suffix
                                        (add1 idx))
                                  (let ((s (integer-suffix->code CH)))
                                    (if s
                                        (ret0 val
                                              (* suffix s)
                                              idx)
                                        (ret0 (+ d (* BASE val))
                                              suffix
                                              (add1 idx))))))))))))
          (cond ((MEMO-REF id #f) => (lambda (v) (return (car v) (cdr v))))
                (else
                 (pptrie.accumulator
                  id
                  (lambda (_ acc)
                    (iter acc
                          (lambda (val type/suff _)
                            (define suffix (code->integer-suffix type/suff))
                            (MEMO-SET! id (cons val suffix))
                            (return val suffix))))))))))

    (define escape-char-value
      (let ((all-escapes (make-hash
                          (map (lambda (a) (cons (char->integer (car a))
                                            (char->integer (cdr a))))
                               `((#\t . #\tab)
                                 (#\v . #\vtab)
                                 (#\r . #\return)
                                 (#\n . #\linefeed)
                                 (#\\ . #\\)
                                 (#\' . #\')
                                 (#\" . #\")
                                 (#\? . #\?)
                                 (#\f . #\page)
                                 (#\a . #\u0007)
                                 (#\b . #\backspace))))))
        (lambda (symbol)
          (hash-ref all-escapes symbol
                    (lambda ()
                      (error "unknown escape char --" symbol))))))

    (define unicode-value4
      (lambda (seq ret)
        "todo == how to compute unicode values"
        (if (>= (length seq) 4)
            (let ((a0 (digit-value (car seq)))
                  (a1 (digit-value (cadr seq)))
                  (a2 (digit-value (caddr seq)))
                  (a3 (digit-value (cadddr seq))))
              (if (and a0 a1 a2 a3)
                  (ret (+ a3
                          (* 256 a2)
                          (* 256 256 a1)
                          (* 256 256 256 a0))
                       (cddddr seq))
                  (PANIC "unicode expects 4 hex digits" a0 a1 a2 a3)))
            (PANIC "unicode expects 4 hex digits. given only" (length seq)))))

    (define unicode-value8
      (lambda (seq ret)
        "todo == how to compute unicode values"
        (if (>= (length seq) 8)
            (unicode-value4
             seq
             (lambda (v1 r1)
               (unicode-value4
                r1
                (lambda (v2 r2)
                  (if (and v1 v2)
                      (ret (+ v2 (* 256 256 256 256 v1))
                           r2)
                      (PANIC "unicode expects 4 hex digits" v1 v2))))))
            (PANIC "unicode expects 4 hex digits. given only" (length seq)))))

    (define octal-escape-sequence? octal-digit-value)
    (define octal-escape-sequence
      (lambda (seq return)
        (let ((x1 (octal-digit-value (car seq)))
              (x2 (and (cons? (cdr seq))
                       (octal-digit-value (cadr seq)))))
          (if x2
              (let ((x3 (and (cons? (cddr seq))
                             (octal-digit-value (caddr seq)))))
                (if x3
                    (return (+ (* 8  8 x1)
                               (* 8 x2)
                               x3)
                            (cdddr seq))
                    (return (+ (* 8 x1)
                               x2)
                            (cddr seq))))
              (return x1
                      (cdr seq))))))

    (define hexadecimal-escape-sequence? (lambda (in) (or (= in HEXAx1) (= in HEXAX2))))
    (define hexadecimal-escape-sequence
      (lambda (seq return)
        ((lambda (s) (s s seq 0))
         (lambda (s seq k)
           (if (digit-value (car seq))
               (s s (cdr seq)
                  (+ (digit-value (car seq))
                     (* 16 k)))
               (return k seq))))))

    (define chval
      (lambda (seq ret)
        (let ((X (car seq)))
          (if (= ESCAPE X)
              ;; ESCAPE
              (if (= UNICODE/u4 (cadr seq))
                  ;; unicode/4
                  (unicode-value4 (cddr seq) ret)
                  (if (= UNICODE/U8 (cadr seq))
                      ;; unicode/8
                      (unicode-value8 (cddr seq) ret)
                      (if (octal-escape-sequence? (cadr seq))
                          (octal-escape-sequence (cdr seq) ret)
                          (if (hexadecimal-escape-sequence? (cadr seq))
                              (hexadecimal-escape-sequence (cddr seq) ret)
                              (ret (escape-char-value (cadr seq))
                                   (cddr seq))))))
              (ret (car seq) (cdr seq))))))

    (define seqvalue
      (lambda (acc retx)
        (define iter/str/char
          (lambda (ACC ret0)
            (if (null? (cdr ACC))
                (ret0 '())
                (chval ACC
                       (lambda (CH rest)
                         (iter/str/char rest
                                        (lambda (accval)
                                          (ret0 (cons CH accval)))))))))

        (define widechar?
          (lambda (seq ret)
            (if (= (car seq) WIDE-PREFIX/L)
                ;; L'mmm'
                (iter/str/char (cddr seq) (lambda (v) (ret v 'WIDE)))
                ;; 'mmm'
                (iter/str/char (cdr seq) (lambda (v) (ret v 'BYTE))))))

        (widechar? acc retx)))

    (define string-value
      (lambda (id return)
        (cond ((MEMO-REF id #f) => (lambda (v) (return (car v) (cdr v))))
              (else
               (pptrie.accumulator
                id
                (lambda (acc _)
                  (seqvalue acc
                            (lambda (v prefix)
                              (MEMO-SET! id (cons v prefix))
                              (return v prefix)))))))))

    (define symbol-value
      (lambda (id return)
        (cond ((MEMO-REF id #f) => return)
              (else
               (pptrie.accumulator
                id
                (lambda (acc _)
                  (define x
                    (string->symbol
                     (list->string (map integer->char acc))))
                  (MEMO-SET! id x)
                  (return x)))))))

    (define float-value
      (lambda (id return)
        (cond ((MEMO-REF id #f) => (lambda (v)
                                     (return (car v) (cdr v))))
              (else
               (pptrie.accumulator
                id
                (lambda (acc _)
                  ((lambda (s) (s s acc
                             (lambda (i f)
                               (MEMO-SET! id (cons i f))
                               (return i f))))
                   (lambda (s acc col)
                     (if (= (car acc) DOT)
                         (col '() (map digit-value (cdr acc)))
                         (s s (cdr acc)
                            (lambda (v rest)
                              (col (cons (digit-value (car acc))
                                         v)
                                   rest)))))) ))))))

    (define char-value
      (lambda (id return)
        (cond ((MEMO-REF id #f) => (lambda (v) (return (car v) (cdr v))))
              (else
               (pptrie.accumulator
                id
                (lambda (acc _)
                  (seqvalue acc
                            (lambda (v prefix)
                              (MEMO-SET! id (cons (car v) prefix))
                              (return (car v) prefix)))))))))

    (lambda (m)
      (case m
        ('INSERT-CHAR      (pptrie 'insert-char!))
        ('NEW!             (pptrie 'new!))
        ('ROOT.IDX         (pptrie'ROOT-NODEIDX))
        ('VALUE.INT.DEC    (integer-value 10 0))
        ('VALUE.INT.OCT    (integer-value 8  1))
        ('VALUE.INT.HEX    (integer-value 16 2))
        ('VALUE.INT.BIN    (integer-value 2  2))
        ('VALUE.STRING     string-value)
        ('VALUE.SYMBOL     symbol-value)
        ('VALUE.CHAR       char-value)
        ('VALUE.FLOAT      float-value)
        (else
         (error "trie" m))))))

(define CTRIE (c_lex_trie_values))

(define pp:dbg
  (lambda (tk)
    (pptrie.accumulator
     (PP.static_lexeme.id tk)
     (lambda (acc _)
       (list->string (map integer->char acc))))))

(define mk.eos
  (lambda (x1 y1 x2 y2 filename)
    (PP.EOS '_ (CTRIE 'ROOT.IDX) x1 y1 x2 y2 '() filename 'a )))

(define month.name
  (vector 'Jan 'Feb 'Mar 'Apr 'May 'Jun
          'Jul 'Aug 'Sep 'Oct 'Nov 'Dec))

(define dynamic_tokdup
  (lambda (CONS type s stok)
    (define id (pptrie.insert-string s))
    (CONS type
          id
          (PP.ID.co_start_x stok)
          (PP.ID.co_start_y stok)
          (PP.ID.co_end_x stok)
          (PP.ID.co_end_y stok)
          (PP.ID.splice stok)
          (PP.ID.filename stok)
          (PP.ID.aux_param stok))))

(define dynamic-macro-object-time
  (lambda (stok)
    "hh:mm:ss"
    (let ((time (seconds->date
                 (* 0.001 (current-inexact-milliseconds)))))
      (let ((s (string-append
                "\""
                (number->string (date-hour time)) ":"
                (number->string (date-minute time)) ":"
                (number->string (date-second time))
                "\"")))
        (dynamic_tokdup PP.STRING '@string s stok)))))

(define dynamic-macro-object-date
  (lambda (stok)
    "Mmm dd yyyy"
    (let ((time (seconds->date
                 (* 0.001 (current-inexact-milliseconds)))))
      (let ((s (string-append
                "\""
                (symbol->string
                 (vector-ref month.name (sub1 (date-month time)))) " "
                 (number->string (date-day time)) " "
                 (number->string (date-year time))
                 "\"")))
        (dynamic_tokdup PP.STRING '@string s stok)))))

(define dynamic-macro-object-file
  (lambda (stok)
    "Mmm dd yyyy"
    (let ((data (PP.ID.filename stok)))
      (let ((s (string-append
                "\""
                (if (string? data)
                    data
                    (path->string data))
                "\"")))
        (let ((id (pptrie.insert-string s)))
          (dynamic_tokdup PP.STRING '@string s stok))))))

(define dynamic-macro-object-line
  (lambda (stok)
    "Mmm dd yyyy"
    (let ((s (number->string
              (PP.ID.co_start_x stok))))
      (dynamic_tokdup PP.INTEGER_DEC '@integer-dec s stok))))

(define .DEFINED                "defined")
(define .STRINGIZE              "#")
(define .CONCATENATION          "##")
(define .ZERO                   "0")
(define .ONE                    "1")
(define .VA_ARGS__              "__VA_ARGS__")
(define .HASHTAG                "#")
;; dynamic identifiers of prep  rocessor
(define .TIME__                 "__TIME__")
(define .DATE__                 "__DATE__")
(define .FILE__                 "__FILE__")
(define .LINE__                 "__LINE__")

(define .COMMA                  ",")
(define .COLON                  ":")
(define .SEMICOLON              ";")
(define .ELLIPSIS               "...")
(define .OPEN_BRACKET           "[")
(define .CLOSE_BRACKET          "]")
(define .OPEN_BRACE             "{")
(define .CLOSE_BRACE            "}")
(define .OPEN_PAREN             "(")
(define .CLOSE_PAREN            ")")
(define .QUESTION               "?")
(define .LOGOR                  "||")
(define .LOGAND                 "&&")
(define .LOGNOT                 "!")
(define .INCLUSIVE_OR           "|")
(define .EXCLUSIVE_OR           "^")
(define .AND                    "&")
(define .NOT                    "~")
(define .EQUAL                  "==")
(define .DIFFERENT              "!=")
(define .LESS_THAN              "<")
(define .GREATER_THAN           ">")
(define .LESS_OR_EQUAL          "<=")
(define .GREATER_OR_EQUAL       ">=")
(define .ASTERISK               "*")
(define .DIVISION               "/")
(define .REMAINDER              "%")
(define .ADDITION               "+")
(define .SUBTRACTION            "-")
(define .SHIFT_LEFT             "<<")
(define .SHIFT_RIGHT            ">>")
(define .DOT                    ".")
(define .ARROW                  "->")
(define .INCREMENTER            "++")
(define .DECREMENTER            "--")
(define .ASSIGN                 "=")
(define .ASSIGN_MUL             "*=")
(define .ASSIGN_DIV             "/=")
(define .ASSIGN_REM             "%=")
(define .ASSIGN_ADD             "+=")
(define .ASSIGN_SUB             "-=")
(define .ASSIGN_SHL             "<<=")
(define .ASSIGN_SHR             ">>=")
(define .ASSIGN_AND             "&=")
(define .ASSIGN_XOR             "^=")
(define .ASSIGN_IOR             "|=")

(define .AUTO                   "auto")
(define .ENUM                   "enum")
(define .RESTRICT               "restrict")
(define .UNSIGNED               "unsigned")
(define .BREAK                  "break")
(define .EXTERN                 "extern")
(define .RETURN                 "return")
(define .VOID                   "void")
(define .CASE                   "case")
(define .SHORT                  "short")
(define .VOLATILE               "volatile")
(define .FOR                    "for")
(define .SIGNED                 "signed")
(define .WHILE                  "while")
(define .CONST                  "const")
(define .GOTO                   "goto")
(define .SIZEOF                 "sizeof")
(define .CONTINUE               "continue")
(define .IF                     "if")
(define .STATIC                 "static")
(define .DEFAULT                "default")
(define .INLINE                 "inline")
(define .STRUCT                 "struct")
(define .DO                     "do")
(define .INT                    "int")
(define .CHAR                   "char")
(define .SWITCH                 "switch")
(define .DOUBLE                 "double")
(define .LONG                   "long")
(define .TYPEDEF                "typedef")
(define .ELSE                   "else")
(define .REGISTER               "register")
(define .UNION                  "union")
(define .FLOAT                  "float")

(include "pptriedata.code")

(define PP.OPERATOR
  (let ((OP (make-hash
             `((,:PPTRIE.punct.LOGAND                . LOGAND)
               (,:PPTRIE.punct.LOGOR                 . LOGOR)
               (,:PPTRIE.punct.OPEN_PAREN            . OPEN-PAREN)
               (,:PPTRIE.punct.CLOSE_PAREN           . CLOSE-PAREN)
               (,:PPTRIE.punct.LOGNOT                . LOGNOT)
               (,:PPTRIE.punct.LESS_OR_EQUAL         . LEQ)
               (,:PPTRIE.punct.GREATER_OR_EQUAL      . GEQ)
               (,:PPTRIE.punct.EQUAL                 . EQ)
               (,:PPTRIE.punct.DIFFERENT             . NEQ)
               (,:PPTRIE.punct.SUBTRACTION           . SUBTRACT)
               (,:PPTRIE.punct.ADDITION              . ADDITION)
               (,:PPTRIE.punct.LESS_THAN             . LT)
               (,:PPTRIE.punct.GREATER_THAN          . GT)
               (,:PPTRIE.punct.QUESTION              . ?Q)
               (,:PPTRIE.punct.COLON                 . ?SEP)
               (,:PPTRIE.punct.SHIFT_LEFT            . SHL)
               (,:PPTRIE.punct.SHIFT_RIGHT           . SHR)
               ))))
    (lambda (trieid)
      (hash-ref OP trieid
                (lambda ()
                  (pptrie.accumulator
                   trieid
                   (lambda (a _)
                     (PANIC "~~PP.OPERATOR~~"
                            (list->string (map integer->char a))))))))))
