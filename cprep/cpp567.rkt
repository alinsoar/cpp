;; -*- mode:scheme ; buffer-read-only:nil -*-

(define output-stream-567
  (lambda (dtok)
    (let ((stok (PP.TOK.tok dtok)))
      (if (PP.NEWLINE? stok)
          'drop-newlines
          (cpp5 stok)))))

(define cpp5
  (lambda (stok)
    "Translation phase 5 -- The conversion of the const char and
string literals"
    (define TRIEID (PP.static_lexeme.id stok))
    (if (PP.STRING? stok)
        ((CTRIE 'VALUE.STRING) (PP.STRING.id stok)
         (lambda (v t)
           (cpp6 stok v t)))
        (if (PP.CHARACTER? stok)
            ((CTRIE 'VALUE.CHAR) (PP.CHARACTER.id stok)
             (lambda (v t)
               (cpp6 (CTOK.INTEGER
                      v (if (eq? t 'WIDE)
                            'wchar
                            (if (eq? t 'BYTE)
                                'char
                                'never))
                      (PP.CHARACTER.co_start_x stok)
                      (PP.CHARACTER.co_start_y stok)
                      (PP.CHARACTER.filename stok))
                     '_ '_)))
            (cpp6 stok '_ '_)))))

(define unify-strings
  (lambda (str* ty* acc)
    (cpp7
     ((if (eq? 'BYTE ty*)
          CTOK.STRING
          (if (eq? 'WIDE ty*)
              CTOK.WSTRING
              'never))
      acc
      (PP.STRING.co_start_x (car str*))
      (PP.STRING.co_start_y (car str*))
      (PP.STRING.filename (car str*))))))

(define cpp6
  (letrec ((on-string
            (let ((acc '())
                  (str* '())
                  (ty* 'nil))
              (lambda (stok execution/set ty)
                (if (PP.STRING? stok)
                    (begin (set! str* (cons stok str*))
                           (set! acc (append acc execution/set))
                           (set! ty* (if (or (eq? 'WIDE ty*)
                                             (eq? 'WIDE ty))
                                         'WIDE
                                         'BYTE)))
                    (begin
                      (unify-strings str* ty* acc)
                      (cpp7 stok)
                      (set! str* '())
                      (set! acc '())
                      (set! ty 'nil)
                      (set! continuation on-non-string))))))
           (on-non-string
            (lambda (stok execution/set ty)
              (if (PP.STRING? stok)
                  (begin (set! continuation on-string)
                         (continuation stok execution/set ty))
                  (cpp7 stok))))
           (continuation on-non-string))
    (lambda (stok execution/set ty)
      "Translation phase 6 -- adjacent string literals are concatenated."
      (continuation stok execution/set ty))))

(define pp-integer->integral-token
  (lambda (val SUFF BASE)
    "convert preprocessing integer tokens to integral tokens."

    (define __panic__
      (lambda ()
        (PANIC "too large value" val ";" SUFF BASE)))

    (case (cons SUFF BASE)
      (((L    . DEC)) (if (<= val __LONG_MAX)
                          'long
                          (if (<= val __LLONG_MAX)
                              'long-long
                              (__panic__))))
      (((L    . OCT)
        (L    . HEX)) (if (<= val __LONG_MAX)
                          'long
                          (if (<= val __ULONG_MAX)
                              'unsigned-long
                              (if (<= val __LLONG_MAX)
                                  'long-long
                                  (if (<= val __ULLONG_MAX)
                                      'unsigned-long-long
                                      (__panic__))))))
      (((LL   . DEC)) (if (<= val __LLONG_MAX)
                          'long-long
                          (__panic__)))
      (((LL   . HEX)
        (LL   . OCT)) (if (<= val __LLONG_MAX)
                          'long-long
                          (if (<= val __ULLONG_MAX)
                              'unsigned-long-long
                              (__panic__))))
      (((U    . DEC)) (if (<= val __UINT_MAX)
                          'unsigned
                          (if (<= val __ULONG_MAX)
                              'unsigned-long
                              (if (<= val __ULLONG_MAX)
                                  'unsigned-long-long
                                  (__panic__)))))
      (((U    . OCT)
        (U    . HEX)) (if (<= val __UINT_MAX)
                          'unsigned
                          (if (<= val __ULONG_MAX)
                              'unsigned-long
                              (if (<= val __ULLONG_MAX)
                                  'unsigned-long-long
                                  (__panic__)))))
      (((ULL  . DEC)) (if (<= val __ULLONG_MAX)
                          'unsigned-long-long
                          (__panic__)))
      (((ULL  . OCT)
        (ULL  . HEX)) (if (<= val __ULLONG_MAX)
                          'unsigned-long-long
                          (__panic__)))
      (((UL   . DEC)) (if (<= val __ULONG_MAX)
                          'unsigned-long
                          (if (<= val __ULLONG_MAX)
                              'unsigned-long-long
                              (__panic__))))
      (((UL   . OCT)
        (UL   . HEX)) (if (<= val __ULONG_MAX)
                          'unsigned-long
                          (if (<= val __ULLONG_MAX)
                              'unsigned-long-long
                              (__panic__))))
      (((NONE . DEC)) (if (<= val __INT_MAX)
                          'int
                          (if (<= val __LONG_MAX)
                              'long
                              (if (<= val __LLONG_MAX)
                                  'long-long
                                  (__panic__)))))
      (((NONE . OCT)
        (NONE . HEX)) (if (<= val __INT_MAX)
                          'int
                          (if (<= val __UINT_MAX)
                              'unsigned
                              (if (<= val __LONG_MAX)
                                  'long
                                  (if (<= val __ULONG_MAX)
                                      'unsigned-long
                                      (if (<= val __LLONG_MAX)
                                          'long-long
                                          (if (<= val __ULLONG_MAX)
                                              'unsigned-long-long
                                              (__panic__))))))))
      (else
       (error "unknown integer type --" SUFF BASE)))))

(define c/lexemes/output 'nil)

(define cpp7
  (letrec ((++
            ((lambda (s) (s s (lambda (v) v)))
             (lambda (s col)
               (lambda (ctok)
                 (if (eq? 'close ctok)
                     (col '())
                     (set! ++
                           (s s
                              (lambda (rest)
                                (col (cons ctok rest)))))))))))
    (lambda (stok)
      "Translation phase 7 -- white-spaces are no more significant.
                         -- preprocessing tokens are converted into tokens." 
      
      (case (stok 'CONSID)
        ('CTOK.STRING
         (++ stok))
        ('CTOK.WSTRING
         (++ stok))
        ('CTOK.INTEGER
         (++ stok))
        ('PP.ID
         (if (eq? '@keyword (PP.ID.class stok))
             (++ ((hash-ref PPTRIE.key.hash (PP.ID.id stok))
                  (PP.ID.co_start_x stok)
                  (PP.ID.co_start_y stok)
                  (PP.ID.filename   stok)))
             ((CTRIE 'VALUE.SYMBOL) (PP.ID.id stok)
              (lambda (v)
                (++ (CTOK.ID v
                             (PP.ID.co_start_x stok)
                             (PP.ID.co_start_y stok)
                             (PP.ID.filename   stok)))))))
        ('PP.PUNCTUATOR
         (if (hash-has-key? PPTRIE.punct.hash (PP.PUNCTUATOR.id stok))
             (++ ((hash-ref PPTRIE.punct.hash (PP.PUNCTUATOR.id stok))
                  (PP.PUNCTUATOR.co_start_x stok)
                  (PP.PUNCTUATOR.co_start_y stok)
                  (PP.PUNCTUATOR.filename   stok)))
             (pptrie.accumulator (PP.static_lexeme.id stok)
                                 (lambda (v _)
                                   (PANIC
                                    "`" (list->string (map integer->char v))
                                    "`" "is not a C lexeme."
                                    (PP.PUNCTUATOR.co_start_x stok)
                                    (PP.PUNCTUATOR.co_start_y stok)
                                    (PP.PUNCTUATOR.filename   stok))))))
        ('PP.INTEGER_DEC
         (++ ((CTRIE 'VALUE.INT.DEC) (PP.INTEGER_DEC.id stok)
              (lambda (v suff)
                (CTOK.INTEGER v (pp-integer->integral-token v suff 'DEC)
                              (PP.INTEGER_DEC.co_start_x stok)
                              (PP.INTEGER_DEC.co_start_y stok)
                              (PP.INTEGER_DEC.filename   stok))))))
        ('PP.INTEGER_HEX
         (++ ((CTRIE 'VALUE.INT.HEX) (PP.INTEGER_HEX.id stok)
              (lambda (v suff)
                (CTOK.INTEGER v (pp-integer->integral-token v suff 'HEX)
                              (PP.INTEGER_HEX.co_start_x stok)
                              (PP.INTEGER_HEX.co_start_y stok)
                              (PP.INTEGER_HEX.filename   stok))))))
        ('PP.INTEGER_OCT
         (++ ((CTRIE 'VALUE.INT.OCT) (PP.INTEGER_OCT.id stok)
              (lambda (v suff)
                (CTOK.INTEGER v (pp-integer->integral-token v suff 'OCT)
                              (PP.INTEGER_OCT.co_start_x stok)
                              (PP.INTEGER_OCT.co_start_y stok)
                              (PP.INTEGER_OCT.filename   stok))))))
        ('PP.INTEGER_BIN
         (++ ((CTRIE 'VALUE.INT.BIN) (PP.INTEGER_BIN.id stok)
              (lambda (v suff)
                "not standard, hope for the best"
                (CTOK.INTEGER v (pp-integer->integral-token v suff 'HEX)
                              (PP.INTEGER_BIN.co_start_x stok)
                              (PP.INTEGER_BIN.co_start_y stok)
                              (PP.INTEGER_BIN.filename   stok))))))
        ('PP.FLOAT
         (++ ((CTRIE 'VALUE.FLOAT)
              (PP.FLOAT.id stok)
              (lambda (integral floating)
                (CTOK.FLOATING integral floating
                               (PP.FLOAT.co_start_x stok)
                               (PP.FLOAT.co_start_y stok)
                               (PP.FLOAT.filename   stok))))))
        ('PP.EOS
         (set! c/lexemes/output ++))
        (else
         (PANIC "cpp7" (stok 'CONSID)))))))

