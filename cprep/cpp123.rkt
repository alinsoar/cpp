;; -*- mode:scheme ; buffer-read-only:nil -*-

(include "../tools/temp-helpers.rkt")

(define C-LEXICAL-GRAMMAR-FILE
  "./cprep/c-lexical-grammar")

(define cpp-get-trie-id
  (lambda (acc drop-count until-whitespace?)
    (define iter
      (lambda (a col)
        (if (or (null? a)
                (and until-whitespace? (whitespace? (car a))))
            (col drop-count)
            (iter (cdr a)
                  (lambda (drop)
                    (if (zero? drop)
                        (begin
                          ((CTRIE 'INSERT-CHAR) (car a))
                          (col 0))
                        (col (sub1 drop))))))))
    ((CTRIE 'NEW!))
    (iter acc
          (lambda (_)
            (pptrie.current-nodeidx)))))

(define pp123dbg
  (lambda (t)
    (pretty-print-columns 200)
    (map (lambda (a)
           (__p "-") (pretty-print (a 'TREE)))
         t)))

(define clexer (make-fastlexer C-LEXICAL-GRAMMAR-FILE))
(define make-preprocessing-tokens
  (lambda (filename return)
    (lex-file-buffer
     filename
     clexer
     (lambda (filelexer stream)
       (pp-token-collector
        filelexer
        filename
        (lambda (t _)
          ((lambda (s) (s s t 0 (lambda (v)
                             (return v)
                             ;;(pp123dbg t)
                             ;; (let ((token-stream (list->vector t)))
                             ;;   (return token-stream))
                             )))
           (lambda (s t n ret)
             (if (pair? t)
                 (s s (cdr t) (add1 n) (lambda (v) (vector-set! v n (car t)) (ret v)))
                 (ret (make-vector n))))))
        (lambda (direct . _)
          (PANIC "expected an #IF directive..." direct)))))))

(define preprocess123
  (let ((table (make-hash)))
    (lambda (file ret)
      ;; (__d "~~~~~~~~~~ PREPROCESS FILE" file "~~~~~~~~~~")
      (cond ((hash-ref table file #f) => ret)
            (else
             (make-preprocessing-tokens
              file
              (lambda (w)
                (hash-set! table file w)
                (ret w))))))))

;;;                         PP NON-GROUPING DIRECTIVES
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define preprocess/line/fun/stringize/op
  (lambda (LEX filename bound-vars ret)
    "# ID"
    ((lambda (s) (s s))
     (lambda (s)
       (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
              (case tok-type
                ((@blank @comment)
                 (s s))
                ((@id @keyword)
                 (define ID (cpp-get-trie-id acc 0 #f))
                 (if (memq ID bound-vars)
                     (ret ID)
                     (PANIC "`#` operator must be followed by a parameter name"
                            co.start.x co.start.y co.end.x co.end.y) ) )
                (else (PANIC "expected parameter identifier after stringize operator"
                             co.start.x co.start.y co.end.x co.end.y filename))))
            (lambda (eos.l eos.c)
              (PANIC "`stringize` operator at the end of stream" eos.l eos.c)))))))

(define preprocess/line/fun/concatenation/op
  (lambda (LEX filename bound-vars ret panic)
    "## tok"
    ((lambda (s) (s s))
     (lambda (s)
       (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
              (case tok-type
                ((@blank @comment)
                 (s s))
                ((@return)
                 (panic))
                (else
                 (define ID (cpp-get-trie-id acc 0 #f))
                 (pp-text-line-token-constructor
                  tok-type ID co.start.x co.start.y co.end.x co.end.y splice filename
                  (lambda (TK)
                    (if (memq ID bound-vars)
                        (ret 'PARAM ID)
                        (ret '+TOK TK) ) )
                  'no-ignore-blank) ) ) )
            panic)))))

(define preprocess/line/define/macro
  (lambda (bound-vars fun?)
    (lambda (LEX filename ret)
      "a minilanguage for defining substitutions of the replacement-lists.
       ~ the language has a few operators and an unlimited # of registers
       ~ each operator acts on registers, there is no stack."

      (define param-idx
        (let ((idx (make-hash (map cons bound-vars (range (length bound-vars))))))
          (lambda (p)
            (hash-ref idx p))))

      (define registers
        (let ((s. (make-hash)) (e. (make-hash)) (p. (make-hash))
              (t. '()) (c. '()))
          (define alloc-next-register
            (let ((reg 0))
              (lambda ()
                (set! reg (add1 reg))
                (sub1 reg))))
          (define allocreg
            (lambda (p h)
              (let ((newreg (alloc-next-register)))
                (hash-set! h p newreg)
                newreg)))
          (lambda (m . a)
            (case m
              ('STRINGIZE
               (define pr (registers 'PARAM (car a)))
               (hash-ref s. pr (lambda () (allocreg pr s.))))
              ('EXPAND
               (define pr (registers 'PARAM (car a)))
               (hash-ref e. pr (lambda () (allocreg pr e.))))
              ('PARAM
               (or (memq (car a) bound-vars) (error "not a param" a))
               (define r (param-idx (car a)))
               (hash-ref p. r (lambda () (allocreg r p.))))
              ('CONS
               ;; each cons needs a new register
               (alloc-next-register))
              ('CONCAT
               (let ((newreg (alloc-next-register)))
                 (set! c. (cons (cons newreg a) c.))
                 newreg))
              ('+TOK
               (let ((newreg (alloc-next-register)))
                 (set! t. (cons (cons newreg (car a)) t.))
                 newreg))
              ('GET
               ((car a) t. s. e. c. p. (alloc-next-register)))))))

      (define SET
        (lambda (dest source)
          (PP.ASSIGN dest source)))

      (define CONS
        (lambda (Ra Rb)
          (PP.CONS Ra Rb)))

      (define emitcode-replacement-list
        (lambda (cons-code reg/result)

          (define DBG
            (lambda m
              (list (PP.DBG (cons '---------- m)))
              ;;`(,(cons 'DBG (cons '---------- m)))
              ))

          (define REGS
            (lambda (reg#)
              `(,(PP.REGS (make-vector reg# #f)))))

          (define TOKS
            (lambda (tok*)
              (map (lambda (t) (SET (car t) (PP.STATICTOK (cdr t)))) tok*)))

          (define STR
            (lambda (str*)
              (hash-map str* (lambda (p reg) (SET reg (PP.STRINGIZE p))))))

          (define EXP
            (lambda (exp*)
              (hash-map exp* (lambda (p reg) (SET reg (PP.EXPAND p))))))

          (define CAT
            (lambda (cat*)
              (map (lambda (t) (SET (car t) (PP.PASTE (cadr t) (caddr t)))) (reverse cat*))))

          (define PAR
            (lambda (par*)
              (hash-map par* (lambda (p reg) (SET reg (PP.ARG p))))))

          (define RET
            (lambda (reg/result)
              `(,(PP.RETURN reg/result))))

          (registers 'GET
                     (lambda (tok* str* exp* cat* par* reg#)
                       (ret
                        (append
                         (DBG "init register vector")
                         (REGS reg#)
                         (DBG "put some parameters in some registers")
                         (PAR par*)
                         (DBG "put each token in some register")
                         (TOKS tok*)
                         (DBG "unique stringize and argument expansion")
                         (STR str*)
                         (EXP exp*)
                         (DBG "code")
                         (CAT cat*)
                         (reverse cons-code)
                         (RET reg/result))
                        reg/result)))))

      (define iter
        (lambda (idx col1 col2)
          (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
                 (define ID (cpp-get-trie-id acc 0 #t))
                 (cond ((memq tok-type '(@blank @comment))
                        (iter idx col1 col2))
                       ;; ========================================
                       ((and fun? (:PPTRIE.pptok.STRINGIZE? ID))
                        (preprocess/line/fun/stringize/op
                         LEX filename bound-vars
                         (lambda (paramid)
                           (let ((NEWREG (registers 'CONS))
                                 (STR (registers 'STRINGIZE paramid)))
                             (iter (add1 idx)
                                   (lambda (rest last/reg)
                                     "# PARAM  last/reg*rest"
                                     (let ((NEW (SET NEWREG (CONS STR last/reg))))
                                       (col1 (cons NEW rest) NEWREG)) )
                                   (lambda _
                                     (PANIC "cannot paste string with anything"
                                            co.start.x co.start.y co.end.x co.end.y
                                            filename) ) )) ) ) )
                       ;; ========================================
                       ((and fun? (:PPTRIE.pptok.CONCATENATION? ID))
                        (preprocess/line/fun/concatenation/op
                         LEX filename bound-vars
                         (lambda (ty2 rand2)
                           (let ((RAND2/REG (registers ty2 rand2)))
                             (iter (add1 idx)
                                   (lambda (rest last/reg)
                                     "## RAND2/REG  last/reg*rest"
                                     (col2 RAND2/REG rest last/reg) )
                                   (lambda (reg/right rest last/reg)
                                     "## RAND2/REG ## reg/right  last/reg*rest"
                                     (let* ((CATREG (registers 'CONCAT RAND2/REG reg/right)))
                                       (col2 CATREG rest last/reg) ) ) ) ) )
                         (lambda _
                           (PANIC "`concat` operator at the end of replacement list"
                                  co.start.x co.start.y co.end.x co.end.y filename) ) ) )
                       ;; ========================================
                       ((eq? tok-type '@return)
                        (let ((EOSREG
                               (registers '+TOK
                                          (mk.eos co.start.x co.start.y
                                                  co.end.x co.end.y
                                                  filename))))
                          (col1 '() EOSREG ) ) )
                       ;; ========================================
                       (else
                        (pp-text-line-token-constructor
                         tok-type ID co.start.x co.start.y co.end.x co.end.y splice filename
                         (lambda (TK)
                           (let* ((NEWREG/CONS (registers 'CONS))
                                  (param? (memq ID bound-vars))
                                  (TOKREG (or param? (registers '+TOK TK))))
                             (and param? (set-box! (PP.static_lexeme.aux_param TK) (param-idx ID)))
                             (iter (add1 idx)
                                   (lambda (rest last/reg)
                                     "CONS (TOK  last/reg*rest)"
                                     (let ((NEW (if param?
                                                    (let* ((R (registers 'EXPAND ID)))
                                                      ;;  param IS EXPANDED
                                                      (SET NEWREG/CONS (CONS R last/reg)))
                                                    ;; not param
                                                    (SET NEWREG/CONS (CONS TOKREG last/reg)))))
                                       (col1 (cons NEW rest) NEWREG/CONS)))
                                   (lambda (reg/right rest last/reg)
                                     "CONS ((TK ## reg/right)  last/reg*rest)"
                                     (let ((NEW (if param?
                                                    (let* ((R (registers 'PARAM ID) )
                                                           (CATREG (registers 'CONCAT R reg/right) ) )
                                                      ;;  param is NOT EXPANDED
                                                      (SET NEWREG/CONS (CONS CATREG last/reg) ) )
                                                    (let* ((CATREG (registers 'CONCAT TOKREG reg/right) ) )
                                                      ;; not param
                                                      (SET NEWREG/CONS (CONS CATREG last/reg) ) ) ) ) )
                                       (col1 (cons NEW rest) NEWREG/CONS) ) ) ) ) )
                         '__) ) ) )
               (lambda (eos.l eos.c)
                 (PANIC "directive at the end of stream" eos.l eos.c filename) ) ) ) )

      (iter 0
            emitcode-replacement-list
            (lambda (_ ___ __)
              "collector for 2-op concat"
              (PANIC "`concat` operator at the beginning of replacement list" ) ) ) ) ) )

(define preprocess/fun/params
  (lambda (LEX filename col)
    "capture param names from #define FUN(p1, p2, p3, ...)"
    (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
           (define ID (cpp-get-trie-id acc 0 #f))
           (cond ((eq? tok-type '@blank)
                  (preprocess/fun/params LEX filename col))
                 ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ((:PPTRIE.punct.ELLIPSIS? ID)
                  (preprocess/fun/params
                   LEX filename
                   (lambda (rest prev _)
                     (if (eq? 'END prev)
                         (col (list :PPTRIE.pptok.VA_ARGS__)
                              'VA_ARGS (lambda (x) (- x)))
                         (PANIC "missing `)` in macro function definition"
                                co.start.x co.start.y co.end.x co.end.y filename)))))
                 ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ((:PPTRIE.punct.COMMA? ID)
                  (preprocess/fun/params
                   LEX filename
                   (lambda (rest prev par#)
                     (if (memq prev '(VA_ARGS ID))
                         (col rest 'COMMA par#)
                         (PANIC "expected ID or VA-ARGS after comma"
                                co.start.x co.start.y co.end.x co.end.y filename)))))
                 ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ((:PPTRIE.punct.CLOSE_PAREN? ID)
                  (col '() 'END (lambda (x) x)))
                 ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ((eq? tok-type '@punct)
                  (PANIC "expected comma or close-paren or ellipsis"
                         co.start.x co.start.y co.end.x co.end.y filename))
                 ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ((memq tok-type '(@keyword @id))
                  (preprocess/fun/params
                   LEX filename
                   (lambda (rest prev par#)
                     (if (memq prev '(END COMMA))
                         (col (cons ID rest) 'ID (lambda (x) (par#(add1 x))))
                         (PANIC "expected comma or close paren after ID"
                                co.start.x co.start.y co.end.x co.end.y filename)))))
                 ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ((eq? tok-type '@return)
                  (PANIC "expected macro function parameters"
                         co.start.x co.start.y co.end.x co.end.y filename))
                 ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 (else
                  (PANIC "wrong macro function definition"
                         co.start.x co.start.y co.end.x co.end.y filename))))
         (lambda (eos.l eos.c)
           (PANIC "directive at the end of stream" eos.l eos.c)))))

(define pp-define-fun-directive
  (lambda (LEX co.start.x co.start.y co.end.x co.end.y filename splice acc return)

    (define check-params!
      (lambda (params prev)
        (define iter
          (lambda (p)
            (or (null? p)
                (if (memq (car p) (cdr p))
                    (PANIC "duplicate parameter in macro function definition"
                           co.start.x co.start.y co.end.x co.end.y filename "--"
                           (list->string ((CTRIE 'DBG) (car p))))
                    (iter (cdr p))))))
        (if (memq prev '(ID END))
            (iter params)
            (if (eq? 'VA_ARGS prev )
                (PANIC "expected ID in macro function definition"
                       co.start.x co.start.y co.end.x co.end.y filename)
                (PANIC "expected comma or close paren in macro function definition"
                       co.start.x co.start.y co.end.x co.end.y filename)))))

    (define ID (cpp-get-trie-id (cdr acc) 0 #t))

    (preprocess/fun/params
     LEX filename
     (lambda (params prev par#)
       (check-params! params prev)
       ((preprocess/line/define/macro params #t)
        LEX filename
        (lambda (DIR __)
          (return
           (lambda (pointer)
             (PP.MACRO_FUN pointer ID params (par# 0) DIR
                           co.start.x co.start.y co.end.x co.end.y filename)))))))))

(define pp-define-obj-directive
  (lambda (LEX co.start.x co.start.y co.end.x co.end.y filename splice acc return)
    (define ID (cpp-get-trie-id acc 0 #t))
    ((preprocess/line/define/macro '( ) #f)
     LEX filename
     (lambda (DIR __)
       (return
        (lambda (pointer)
          (PP.MACRO_OBJ pointer ID DIR
                        co.start.x co.start.y co.end.x co.end.y filename)))))))

(define pp-undef-directive
  (lambda (LEX co.start.x0 co.start.y0 co.end.x0 co.end.y0 filename splice acc return)
    (define ID (cpp-get-trie-id acc 0 #t))
    (return
     (lambda (pointer)
       (PP.UNDEF pointer ID co.start.x0 co.start.y0 co.end.x0 co.end.y0
                 filename)))))

(define pp-error-directive
  (lambda (LEX co.start.x0 co.start.y0 co.end.x0 co.end.y0 filename splice acc return)

    (define preprocess/line
      (lambda (col)
        (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
               (if (eq? tok-type '@return)
                   (col '())
                   (preprocess/line
                    (lambda (rest)
                      (if (or (eq? '@blank tok-type)
                              (eq? '@comment tok-type))
                          (col rest)
                          (let ((ID (cpp-get-trie-id acc 0 #f)))
                            (pp-text-line-token-constructor
                             tok-type ID
                             co.start.x co.start.y co.end.x co.end.y
                             splice filename
                             (lambda (TK)
                               (col (cons TK rest) ))
                             '__))
                          )))))
             (lambda (eos.l eos.c)
               (PANIC "directive at the end of stream" eos.l eos.c)))))

    (preprocess/line
     (lambda (err)
       (return
        (lambda (pointer)
          (PP.ERROR pointer err co.start.x0 co.start.y0 co.end.x0 co.end.y0
                    filename)))))))

(define pp-include-directive
  (lambda (LEX co.start.x0 co.start.y0 co.end.x0 co.end.y0 filename splice acc return)

    (define directive-name
      (lambda (col)
        (lambda (ch point spl next)
          "step2: collect header name"
          (if ch
              (next (directive-name
                     (lambda (header)
                       (if (eq? header '__)
                           (col '())
                           (col (cons ch header))))))
              (col '__)))))

    (define preprocess/line
      (lambda (col)
        (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
               (if (eq? tok-type '@return)
                   (col '())
                   (preprocess/line
                    (lambda (rest)
                      (if (or (eq? '@blank tok-type)
                              (eq? '@comment tok-type))
                          (col rest)
                          (col (cons tok-type rest)))))))
             (lambda (eos.l eos.c)
               (PANIC "directive at the end of stream" eos.l eos.c)))))

    (define type-and-headername
      (lambda (ret)
        (let ((type (if (null? acc)
                        'EXPRESSION
                        (if (= (car acc) PARAGRAPH)
                            'LOCAL
                            (if (= (car acc) GREATERTHAN)
                                'SYSTEM
                                (lambda () (error "never")))))))
          (ret type (and (cons? acc)
                         (cpp-get-trie-id (cdr acc) 1 #t))))))

    (type-and-headername
     (lambda (type headername)
       (preprocess/line
        (lambda (DIR)
          (case type
            ('LOCAL
             (or (null? DIR)
                 (WARNING "#include local header"
                          co.start.x0 co.start.y0 co.end.x0 co.end.y0))
             (return
              (lambda (pointer)
                (PP.INCLUDE_LOCAL
                 pointer headername co.start.x0 co.start.y0 co.end.x0 co.end.y0
                 filename))))
            ('SYSTEM
             (or (null? DIR)
                 (WARNING "#include system header"
                          co.start.x0 co.start.y0 co.end.x0 co.end.y0))
             (return
              (lambda (pointer)
                (PP.INCLUDE_SYSTEM
                 pointer headername co.start.x0 co.start.y0 co.end.x0 co.end.y0
                 filename))))
            ('EXPRESSION
             (return
              (lambda (pointer)
                (PP.INCLUDE_EXPRESSION
                 pointer DIR co.start.x0 co.start.y0 co.end.x0 co.end.y0
                 filename)))))))))))

;;;                             PP GROUPING DIRECTIVES
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define preprocess/line/if/defined/op
  (lambda (LEX filename ret)
    "`defined ID` or `defined(ID)`"

    (define read1
      (lambda (ret1 retopenparen retclosparen)
        (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
               (define panic
                 (lambda ()
                   (PANIC "expected `defined ID` or `defined(ID)`"
                          co.start.x co.start.y co.end.x co.end.y filename)))
               (case tok-type
                 ((@blank @comment)
                  (read1 ret1 retopenparen retclosparen))
                 ((@id @keyword)
                  (ret1 (cpp-get-trie-id acc 0 #f) panic))
                 (else
                  (if (:PPTRIE.punct.OPEN_PAREN? (cpp-get-trie-id acc 0 #f))
                      (retopenparen panic)
                      (if (:PPTRIE.punct.CLOSE_PAREN? (cpp-get-trie-id acc 0 #f))
                          (retclosparen panic)
                          (panic))))))
             (lambda (eos.l eos.c)
               (PANIC "`defined` operator at the end of stream" eos.l eos.c)))))
    (read1 (lambda (ID _)
             "defined ID"
             (ret ID))
           (lambda _
             "defined (ID)"
             (read1 (lambda (ID _)
                      (read1 (lambda (_ panic) (panic))
                             (lambda (panic) (panic))
                             (lambda _ (ret ID))))
                    (lambda (panic) (panic))
                    (lambda (panic) (panic))))
           (lambda (panic) (panic)))))

(define preprocess/line/if/conditional/expression
  (lambda (LEX filename ret)

    (define iter
      (lambda (idx defined-idx col)
        (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
               (define ID (cpp-get-trie-id acc 0 #f))
               (if (:PPTRIE.pptok.DEFINED? ID)
                   (preprocess/line/if/defined/op
                    LEX filename
                    (lambda (id)
                      (iter (add1 idx) (cons idx defined-idx)
                            (lambda (rest defidx)
                              (col (cons
                                    (PP.DEFINED '.. id co.start.x co.start.y co.end.x co.end.y
                                                splice filename (vector '?))
                                    rest)
                                   defidx)
                              '__))))
                   (if (or (eq? tok-type '@blank)
                           (eq? tok-type '@comment))
                       (iter idx defined-idx col)
                       (if (eq? tok-type '@return)
                           (col (list (mk.eos 0 0 0 0 filename))
                                defined-idx)
                           (iter (add1 idx) defined-idx
                                 (lambda (rest defidx)
                                   (pp-text-line-token-constructor
                                    tok-type
                                    ID
                                    co.start.x co.start.y co.end.x co.end.y
                                    splice filename
                                    (lambda (TK)
                                      (col (cons TK rest)
                                           defidx))
                                    '__)))))))
             (lambda (eos.l eos.c)
               (PANIC "directive at the end of stream" eos.l eos.c)))))

    (iter 0 '() ret)))

(define pp-if-directive
  (lambda (LEX co.start.x co.start.y co.end.x co.end.y filename splice acc return)
    "todo:add splice to constructor"
    (preprocess/line/if/conditional/expression
     LEX filename
     (lambda (dir defined-indexes)
       (define constant-expression (list->vector dir))
       (return
        (lambda (pointer pointer-jmp)
          (PP.IF pointer pointer-jmp defined-indexes constant-expression
                 co.start.x co.start.y co.end.x co.end.y filename)))))))

(define pp-ifdef/ifndef-directive
  (lambda (pp)
    (lambda (LEX co.start.x co.start.y co.end.x co.end.y filename splice acc return)

      (define preprocess/line
        (lambda (col)
          (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
                 (if (eq? tok-type '@return)
                     (col '())
                     (preprocess/line
                      (lambda (rest)
                        (col (cons tok-type rest))))))
               (lambda (eos.l eos.c)
                 (PANIC "directive at the end of stream" eos.l eos.c)))))

      (define ID (cpp-get-trie-id acc 0 #t))
      (preprocess/line
       (lambda (dir)
         (return
          (lambda (pointer pointer-jmp)
            ((case pp
               ('@pp-ifdef PP.IFDEF)
               ('@pp-ifndef PP.IFNDEF))
             pointer pointer-jmp ID
             co.start.x co.start.y co.end.x co.end.y filename))))))))

(define pp-else-directive
  (lambda (LEX co.start.x co.start.y co.end.x co.end.y filename splice acc return)

    (define preprocess/line
      (lambda (col)
        (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
               (if (eq? tok-type '@return)
                   (col '())
                   (preprocess/line
                    (lambda (rest)
                      (col (cons
                            tok-type
                            rest))))))
             (lambda (eos.l eos.c)
               (PANIC "directive at the end of stream" eos.l eos.c)))))

    (preprocess/line
     (lambda (dir)
       (return
        (lambda (pointer)
          (PP.ELSE pointer co.start.x co.start.y co.end.x co.end.y filename))
        (lambda (pointer pointer-endif)
          (PP.JMP (- pointer pointer-endif)
                  co.start.x co.start.y co.end.x co.end.y filename)))))))

(define pp-elif-directive
  (lambda (LEX co.start.x co.start.y co.end.x co.end.y filename splice acc return)
    (preprocess/line/if/conditional/expression
     LEX filename
     (lambda (dir defined-indexes)
       (define constant-expression (list->vector dir))
       (return (lambda (pointer pointer-jmp)
                 (PP.IF pointer pointer-jmp defined-indexes constant-expression
                        co.start.x co.start.y co.end.x co.end.y filename))
               (lambda (pointer pointer-endif)
                 (PP.JMP (- pointer pointer-endif)
                         co.start.x co.start.y co.end.x co.end.y filename)))))))

(define pp-endif-directive
  (lambda (LEX co.start.x co.start.y co.end.x co.end.y filename splice acc return)
    (define preprocess/line
      (lambda (col)
        (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
               (if (eq? tok-type '@return)
                   (col '())
                   (preprocess/line
                    (lambda (rest)
                      (col (cons
                            tok-type
                            rest))))))
             (lambda (eos.l eos.c)
               (PANIC "directive at the end of stream" eos.l eos.c)))))
    (preprocess/line
     (lambda (dir)
       (return (lambda (pointer)
                 (PP.ENDIF pointer
                           co.start.x co.start.y co.end.x co.end.y filename)))))))

;;;                                          PP TOKENS
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define pp-text-line-token-constructor
  (lambda (tok-class
      ID
      co.start.x co.start.y co.end.x co.end.y
      splice filename
      return ignore-blank)
    (let ((KONS (case tok-class
                  ('@blank              'ignore-blank)
                  ('@comment            'ignore-blank)
                  ('@ignore             'ignore-blank)
                  ('@return             PP.NEWLINE)
                  ('@id                 PP.ID)
                  ('@keyword            PP.ID)
                  ('@punct              PP.PUNCTUATOR)
                  ('@ct-float           PP.FLOAT)
                  ('@pp-numeric         PP.NUMERIC)
                  ('@string             PP.STRING)
                  ('@ct-integer-dec     PP.INTEGER_DEC)
                  ('@ct-integer-oct     PP.INTEGER_OCT)
                  ('@ct-integer-hex     PP.INTEGER_HEX)
                  ('@ct-integer-bin     PP.INTEGER_BIN)
                  ('@ct-char            PP.CHARACTER)
                  ('@pp-include         PP.NOLEXEME)
                  ('@pp-endif           PP.NOLEXEME)
                  ('@nonclosed-comment  'nonclosed-comment)
                  ('@nonclosed-string   'nonclosed-string)
                  ('@pp-impl            (__d "#warning:todo") 'ignore-blank)
                  ;; ('@gnu-key        (WARN class "This is GNU extension.")
                  (else                 (PANIC "token-constructor: unknown token type --"
                                               tok-class ";" co.start.x co.start.y filename)
                                        'nil))))

      (define aux
        (lambda ()
          (box #f)))

      (if (eq? KONS 'ignore-blank)
          (ignore-blank)
          (if (eq? KONS 'nonclosed-comment)
              (PANIC "nonclosed comment" co.start.x co.start.y filename)
              (if (eq? KONS 'nonclosed-string)
                  (PANIC "nonclosed string" co.start.x co.start.y filename)
                  (or (eq? 'nil KONS)
                      (return
                       (KONS tok-class
                             ID
                             co.start.x co.start.y co.end.x co.end.y
                             splice filename
                             (aux) ) ) ) ) ) ) ) ) )

(define tokenize-text-line
  (lambda (LEX filename colrest ret)
    (define iter
      (lambda (col)
        (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
               (define ID (cpp-get-trie-id acc 0 #f))
               (pp-text-line-token-constructor
                tok-type ID
                co.start.x co.start.y co.end.x co.end.y
                splice filename
                (lambda (TK)
                  (if (eq? tok-type '@return)
                      (ret  (lambda (rest pointer) (col (cons TK rest) (add1 pointer))))
                      (iter (lambda (rest pointer) (col (cons TK rest) (add1 pointer))))))
                (lambda ()
                  "doen't collect blanks"
                  (iter col))))
             (lambda (EOFpos.L EOFpos.C)
               "end of file"
               (col (mk.eos EOFpos.L EOFpos.C 0 0 filename)
                    0)))))
    (iter colrest)))

;;;                                         MAIN LOOP
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define pp-token-collector
  (lambda (LEX filename col conditional-group)
    "each line preprocessing starts here.
     this first token of each line dispatches between
     preprocessing directives and text code line."
    (LEX (lambda (tok-type co.start.x co.start.y co.end.x co.end.y splice acc)
           "this is the first token in the preprocessing line."
           ;; (__d "." tok-type (map (lambda (a)
           ;;                          (if (< a 32)
           ;;                              a
           ;;                              (integer->char a)))
           ;;                        acc))
           (case tok-type
             ((@blank @comment)
              "some blank at the very beginning of line"
              (pp-token-collector LEX filename col conditional-group))
             ((@return)
              "an empty line -- the same as `else-branch`
               but no tokenize-text-line"
              (pp-token-collector
               LEX filename
               (lambda (rest pointer)
                 (define ID (cpp-get-trie-id acc 0 #f))
                 (pp-text-line-token-constructor
                  tok-type ID
                  co.start.x co.start.y co.end.x co.end.y splice filename
                  (lambda (nltok)
                    (col (cons nltok rest) (add1 pointer)))
                  (lambda ()
                    (col rest pointer))))
               conditional-group))
             ((@pp-if @pp-ifdef @pp-ifndef)
              ((case tok-type
                 ('@pp-if                pp-if-directive)
                 ((@pp-ifdef @pp-ifndef) (pp-ifdef/ifndef-directive tok-type)))
               LEX co.start.x co.start.y co.end.x co.end.y
               filename splice acc
               (lambda (DIRECT)
                 (let ((pointer-jmp 'nil))
                   (pp-token-collector
                    LEX filename
                    (lambda (rest pointer)
                      (if (eq? 'nil pointer-jmp)
                          (PANIC "IF block -- ENDIF directive is missing."
                                 co.start.x co.start.y co.end.x co.end.y filename)
                          (col (cons (DIRECT pointer pointer-jmp) rest)
                               (add1 pointer))))
                    (lambda (sender-block-type ret)
                      (case sender-block-type
                        ((ENDIF)
                         (ret (lambda (pointer0)
                                (set! pointer-jmp pointer0))
                              conditional-group))
                        ((ELSE ELIF)
                         (ret (lambda (pointer0 __)
                                (set! pointer-jmp pointer0))
                              conditional-group)))))))))
             ((@pp-else)
              (pp-else-directive
               LEX co.start.x co.start.y co.end.x co.end.y
               filename splice acc
               (lambda (DIRECT DIRECT/JMP)
                 (conditional-group
                  'ELSE
                  (lambda (setter parent-conditional-group)
                    (let ((pointer-endif 'nil))
                      (pp-token-collector
                       LEX filename
                       (lambda (rest pointer)
                         (setter pointer pointer-endif)
                         (let ((else-direct (DIRECT pointer))
                               (jump (DIRECT/JMP (add1 pointer) pointer-endif)))
                           (if (eq? 'nil pointer-endif)
                               (PANIC
                                "ELSE block -- ENDIF directive is missing.")
                               (col (cons jump (cons else-direct rest))
                                    (add1 (add1 pointer))))))
                       (lambda (sender-block-type ret)
                         (case sender-block-type
                           ((ENDIF)
                            (ret (lambda (pointer0)
                                   (set! pointer-endif pointer0))
                                 parent-conditional-group))
                           (else (PANIC "cannot use" sender-block-type
                                        "block inside an ELSE block")))))))))))
             ((@pp-elif)
              (pp-elif-directive
               LEX co.start.x co.start.y co.end.x co.end.y
               filename splice acc
               (lambda (DIRECT DIRECT/JMP)
                 (conditional-group
                  'ELIF
                  (lambda (setter parent-conditional-group)
                    (let ((pointer-endif 'nil)
                          (pointer-jmp 'nil))
                      (pp-token-collector
                       LEX filename
                       (lambda (rest pointer)
                         (setter pointer pointer-endif)
                         (let ((elif-direct (DIRECT pointer pointer-jmp))
                               (jump (DIRECT/JMP (add1 pointer) pointer-endif)))
                           (if (eq? 'nil pointer-endif)
                               (PANIC
                                "ELSE IF block -- ENDIF directive is missing.")
                               (col (cons jump (cons elif-direct rest))
                                    (add1 (add1 pointer))))))
                       (lambda (sender-block-type ret)
                         (case sender-block-type
                           ((ENDIF)
                            (ret (lambda (pointer0)
                                   (set! pointer-jmp pointer0)
                                   (set! pointer-endif pointer0))
                                 parent-conditional-group))
                           ((ELSE ELIF)
                            (ret (lambda (pointer0 pointer1)
                                   (set! pointer-jmp pointer0)
                                   (set! pointer-endif pointer1))
                                 parent-conditional-group))
                           (else (PANIC "cannot use" sender-block-type
                                        "block inside an ELIF block")))))))))))
             ((@pp-endif)
              (pp-endif-directive
               LEX co.start.x co.start.y co.end.x co.end.y
               filename splice acc
               (lambda (DIRECT)
                 (conditional-group
                  'ENDIF
                  (lambda (setter parent-conditional-group)
                    (pp-token-collector
                     LEX filename
                     (lambda (rest pointer)
                       (setter pointer)
                       (col (cons (DIRECT pointer) rest)
                            (add1 pointer)))
                     parent-conditional-group))))))
             ((@pp-define-fun @pp-define-obj @pp-include @pp-undef @pp-error)
              "non-grouping directives"
              (define DIR
                (case tok-type
                  ('@pp-define-fun pp-define-fun-directive)
                  ('@pp-define-obj pp-define-obj-directive)
                  ('@pp-include    pp-include-directive)
                  ('@pp-undef      pp-undef-directive)
                  ('@pp-error      pp-error-directive)))
              (DIR LEX co.start.x co.start.y co.end.x co.end.y
                   filename splice acc
                   (lambda (DIRECT)
                     (pp-token-collector
                      LEX filename
                      (lambda (rest pointer)
                        (col (cons (DIRECT pointer) rest) (add1 pointer)))
                      conditional-group))))
             (else
              "TEXT LINE including empty line"
              (define ID (cpp-get-trie-id acc 0 #f))
              (tokenize-text-line
               LEX filename
               (lambda (rest pointer)
                 (pp-text-line-token-constructor
                  tok-type
                  ID
                  co.start.x co.start.y co.end.x co.end.y
                  splice filename
                  (lambda (TK)
                    (col (cons TK rest) (add1 pointer)))
                  (lambda ()
                    (col rest pointer))))
               (lambda (col)
                 (pp-token-collector LEX filename col conditional-group))))))
         (lambda (EOFpos.L EOFpos.C)
           "end of file"
           (col (list (mk.eos EOFpos.L EOFpos.C 0 0 filename))
                0)))))
