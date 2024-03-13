;; -*- mode:scheme ; buffer-read-only:nil -*-

;;; MACRO EXPANSION -- PROSSER'S ALGORITHM -- X3J11/86-196
;;; https://www.spinellis.gr/blog/20060626/x3J11-86-196.pdf
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define cpp/dbg/dyntok
  (lambda (dtok)
    "only for debugging"
    (if (PP.PLACEMAKER_4th? dtok)
        "placemaker4"
        (let ((t (pp:dbg (PP.TOK.tok dtok)))
              (b (set->list (pp-blue-set-get dtok))))
          (if (PP.NEWLINE? (PP.TOK.tok dtok))
              "\\n"
              (~a "=`" t "." (PP.static_lexeme.id (PP.TOK.tok dtok))
                  ":" b ";  "))))))

(define cpp-stringize-op
  (lambda (arg* ret)
    (define STR
      (lambda (acc)
        "for char and srt types"
        ((lambda (s) (s s acc))
         (lambda (s acc)
           "backslash => backslash backslash
            paragraph => backslash paragraph
            identity for the rest"
           (if (null? acc)
               '()
               (let ((x (car acc)))
                 (if (or (eq? x BS)
                         (eq? x PARAGRAPH))
                     (cons BS (cons x (s s (cdr acc))))
                     (cons x (s s (cdr acc))))))))))
    (define str0
      (map (lambda (a)
             (if (PP.PLACEMAKER_4th? a)
                 ""
                 (let ((tk (PP.TOK.tok a)))
                   (pptrie.accumulator
                    (PP.static_lexeme.id tk)
                    (lambda (acc _)
                      (let ((ty (PP.static_lexeme.class tk)))
                        (if (or (eq? ty '@string)
                                (eq? ty '@ct-char))
                            ;; str or char
                            (STR acc)
                            acc)))))))
           arg*))
    ;; (__d "A" str0 (map string? str0))
    ;; (__d "?" (apply string-append str0))
    (define str
      (pptrie.insert-string
       (string-append "\"" (list->string
                            (map integer->char
                                 (apply append str0)))
                      "\"")))
    (ret (PP.STRING '@string str 0 0 0 0 #f ":str" (box #f)))))

(define cpp-paste-op
  (lambda (op1 op2 ret)
    (define buffer (string-append (pp:dbg (PP.TOK.tok op1)) (pp:dbg (PP.TOK.tok op2))))
    (define lex (clexer (list->vector
                         (map char->integer
                              (string->list buffer))) 0))
    (define paste-fail
      (lambda _
        (PANIC "cannot paste"
               ((PP.TOK.tok op1) 'TREE)
               ((PP.TOK.tok op2) 'TREE))))
    (lex
     (lambda (tok-type ..m __o _r ._s _.e ...ignored)
       (lex paste-fail
            (lambda _
              (define ID (pptrie.insert-string buffer))
              (pp-text-line-token-constructor
               tok-type ID -1 -1 -1 -1 #f ":pasteop"
               (lambda (TK)
                 (ret 
                  ;; Nota bene -- Prosser's algorithm requires a new
                  ;; token to be created with (set-intersect
                  ;; (pp-blue-set-get op1) (pp-blue-set-get op2))
                  ;; while GCC implementation does it with an empty
                  ;; blue paint set.
                  (PP.TOK TK (cpp-blue-set-empty) (PP.TOK.env op1))))
               'no-blank))))
     paste-fail)))

(define cpp-concatenation-op
  (lambda (arg1* arg2* ret)
    (define A1-BEGIN (drop-right arg1* 1))
    (define A1-LAST (last arg1*))
    (define A2-FIRST (car arg2*))
    (define A2-REST (cdr arg2*))
    (define pm1 (PP.PLACEMAKER_4th? A1-LAST))
    (define pm2 (PP.PLACEMAKER_4th? A2-FIRST))
    (cond ((and pm1 pm2)
           (if (and (null? A1-BEGIN) (null? A2-REST))
               (ret (list A1-LAST))
               (ret (append A1-BEGIN A2-REST))))
          (pm1 (ret (append A1-BEGIN arg2*)))
          (pm2 (ret (append arg1* A2-REST)))
          (else
           (cpp-paste-op A1-LAST A2-FIRST
                         (lambda (o)
                           (ret (append A1-BEGIN (list o) A2-REST))))))))

(define cpp-macro-replacement-list-evaluator
  (lambda (code args/vector env ret)
    (define reg/vector 'nil)
    
    (define compute_value
      (lambda (instr ret)
        (case (instr 'CONSID)
          ('PP.ARG       (ret (vector-ref args/vector (PP.ARG.idx instr))))
          ('PP.STATICTOK (let ((tok (PP.STATICTOK.tok instr)))
                           (ret (if (PP.EOS? tok) '()
                                    (PP.TOK tok
                                            (cpp-blue-set-empty)
                                            env)))))
          ('PP.STRINGIZE (cpp-stringize-op
                          (vector-ref reg/vector (PP.STRINGIZE.idx instr))
                          (lambda (s)
                            (ret (PP.TOK s (cpp-blue-set-empty) env)))))
          ('PP.EXPAND    (letrec ((a (vector-ref reg/vector (PP.EXPAND.reg instr)))
                                  (res '())
                                  (output 
                                   (lambda (t)
                                     (if (eq? 'end t)
                                         (ret (reverse (cdr res)))
                                         (set! res (cons t res))))))
                           (if (PP.PLACEMAKER_4th? (car a))
                               (ret a)
                               ((cpp-macro-expand output)
                                (car a)
                                ((lambda (s) (s s (cdr a)))
                                 (lambda (s a*)
                                   (lambda (next)
                                     (if (null? a*)
                                         (next (PP.TOK 
                                                (mk.eos 0 0 0 0 ":dummy;end of expansion")
                                                (cpp-blue-set-empty)
                                                env)
                                               (lambda (?)
                                                 (output 'end)))
                                         (next (car a*)
                                               (s s (cdr a*)))))))))))
          ('PP.CONS      (let* ((reg1 (PP.CONS.reg1 instr))
                                (reg2 (PP.CONS.reg2 instr))
                                (v1 (vector-ref reg/vector reg1))
                                (v2 (vector-ref reg/vector reg2))
                                (x1 (if (list? v1) v1 (list v1)))
                                (x2 (if (list? v2) v2 (list v2))))
                           ;; todo: with a little optimization
                           ;; `append` call can be replaced by `cons`
                           ;; in cpp123
                           (ret (append x1 x2))))
          ('PP.PASTE     (let* ((reg1 (PP.PASTE.reg1 instr))
                                (reg2 (PP.PASTE.reg2 instr))
                                (v1 (vector-ref reg/vector reg1))
                                (v2 (vector-ref reg/vector reg2))
                                (x1 (if (list? v1) v1 (list v1)))
                                (x2 (if (list? v2) v2 (list v2))))
                           (cpp-concatenation-op x1 x2 ret)))
          (else (error "?" (instr 'CONSID))))))
    
    ((lambda (s) (s s code))
     (lambda (s code)
       (if (null? code)
           (ret 'OK)
           (let ((instr (car code)))
             ;; (__d ".exe" (instr 'TREE))
             (case (instr 'CONSID)
               ('PP.REGS     (set! reg/vector (PP.REGS.reg_vector instr))
                             (s s (cdr code)))
               ('PP.DBG      'disabled
                             ;; '(map __p (cdr (PP.DBG.msg instr)))
                             ;; '(newline)
                             (s s (cdr code)))
               ('PP.ASSIGN   (let ((reg (PP.ASSIGN.reg instr))
                                   (value (PP.ASSIGN.val instr)))
                               (compute_value (PP.ASSIGN.val instr)
                                              (lambda (val)
                                                (vector-set! reg/vector reg val)
                                                (s s (cdr code))))))
               ('PP.RETURN   (let ((reg (PP.RETURN.reg instr)))
                               (ret (vector-ref reg/vector reg))))
               (else
                (s s (cdr code))))))))))

(define cpp-macro-substitution
  (lambda (IS FP AP BS OS env)
    "substitute args, handle stringize and paste"
    ;; (__d ".subst")
    
    (define bsadd
      (lambda (os)
        "blue set add"
        (if (null? os)
            '()
            (if (PP.PLACEMAKER_4th? (car os))
                ;; clean placemakers of the 4th stage
                (bsadd (cdr os))
                (cons (pp-blue-set-add! (car os) BS)
                      (bsadd (cdr os))))))) 
    
    (cpp-macro-replacement-list-evaluator
     IS AP env
     (lambda (os__)
       (OS (bsadd os__))))))

(define function-call-try-collect-arguments
  (lambda (funid param-count input-stream succ panic)
    (define placemaker4
      (lambda ()
        (list (PP.PLACEMAKER_4th '@cpp4 0 -1 -1 -1 -1 #f ":placemaker" (box #f)))))
    (define iter
      (lambda (next-input nest idx col)
        "idx is the index of the current parameter.  idx shrinks each
         time comma separates a new argument.  when idx is negative we
         need an ellipsis operator and access its last argument with
         __VA_ARGS__ macro."
        (next-input
         (lambda (dtok next-input0)
           (define stok (PP.TOK.tok dtok))
           (define ID (PP.static_lexeme.id stok))
           (cond ((PP.EOS? stok)
                  (PANIC "non-closed-macro-call"))
                 ;; ========================================
                 ((and (zero? nest) (:PPTRIE.punct.CLOSE_PAREN? ID))
                  (col '() '() next-input0 dtok idx))
                 ;; ========================================
                 ((and (zero? nest) (:PPTRIE.punct.COMMA? ID))
                  (iter next-input0 nest (sub1 idx)
                        (lambda (a0 a* input close/paren p#)
                          (if (positive? idx)
                              ;; start collecting idxTH argument
                              (if (null? a0)
                                  (col '() (cons (placemaker4) a*) input close/paren p#)
                                  (col '() (cons a0 a*) input close/paren p#))
                              ;; more arguments than parameters;
                              ;; collect them all together as the last
                              ;; `__VA_ARGS__` argument
                              (col (cons dtok a0) '() input close/paren p#)))))
                 ;; ========================================
                 ((:PPTRIE.punct.OPEN_PAREN? ID)
                  (iter next-input0 (add1 nest) idx
                        (lambda (a0 a* input close/paren p#)
                          (col (cons dtok a0) a* input close/paren p#))))
                 ;; ========================================
                 ((PP.NEWLINE? stok)
                  (iter next-input0 nest idx col))
                 ;; ========================================
                 ((:PPTRIE.punct.CLOSE_PAREN? ID)
                  (iter next-input0 (sub1 nest) idx
                        (lambda (a0 a* input close/paren p#)
                          (col (cons dtok a0) a* input close/paren p#))))
                 ;; ========================================
                 (else
                  (iter next-input0 nest idx
                        (lambda (a0 a* input close/paren p#)
                          (col (cons dtok a0) a* input close/paren p#)))))))))
    
    (iter input-stream 0 (abs param-count)
          (lambda (a0 a* input close/paren p#)
            (define args (cons (if (null? a0) (placemaker4) a0) a*))
            (define p/count (if (equal? '(()) args) p# (sub1 p#)))
            (if (or (zero? p/count)
                    (and (zero? param-count)
                         (= 1 (length args))
                         (PP.PLACEMAKER_4th? (caar args)))
                    (and (negative? p/count)
                         (negative? param-count)))
                (succ (list->vector args)
                      input close/paren)
                (PANIC "function macro"
                       (pptrie.accumulator funid
                                           (lambda (m _)
                                             (~a "`"(list->string m) "`")))
                       "called with" (- param-count p/count) "argument/s. Needed"
                       (if (negative? param-count) "at least" "")
                       (abs param-count) "argument/s. --" (panic) ) ) ) ) ) )

(define cpp-blue-set-empty
  (lambda ()
    (box (set) ) ) )
(define pp-is-blue-set?
  (lambda (tok) 
    ;; (eq? 'BLUE (pp-blue-set-get tok ) )
    (set-member? (pp-blue-set-get tok) (PP.static_lexeme.id (PP.TOK.tok tok)))))
(define pp-blue-set-get
  (lambda (tok)
    (unbox (PP.TOK.blue_paint tok) ) ) )
(define pp-blue-set-add!
  (lambda (tok new)
    (let ((id (PP.static_lexeme.id (PP.TOK.tok tok))))
      (set-box! (PP.TOK.blue_paint tok)
                (set-union new (pp-blue-set-get tok))
                ;; (if (set-member? new id)
                ;;     'BLUE
                ;;     (set-union new (pp-blue-set-get tok)))
                )
      tok) ) )

(define funcall:dbg
  (lambda (arg* stok)
    "only for debugging"
    (__d ".funcall" (pp:dbg stok) (vector-length arg*))
    (vector-map 
     (lambda (a)
       (__d "----" (length a)
            ":" (map cpp/dbg/dyntok a)
            ";"))
     arg*)
    (__d "_")))

(define cpp-macro-expand
  (lambda (output-stream)
    "recur, substitute, pushback, rescan" 
    
    (define ts
      (lambda (X env re.fun re.obj)
        "replacement token sequence"
        (if (PP.MACRO_FUN? X)
            (re.fun (PP.MACRO_FUN.replacement_tok_seq X))
            (if (PP.MACRO_OBJ? X)
                (re.obj (PP.MACRO_OBJ.replacement_tok_seq X))
                (error "does not happen")))))
    
    (define substituted-stream
      (lambda (input-continuation self)
        (lambda (substituted-stream)
          "generate the stream SUBSTITUTED-STREAM ++ INPUT-CONTINUATION"
          (if (null? substituted-stream)
              (input-continuation self)
              ((lambda (s) (s s (car substituted-stream)
                         (cdr substituted-stream)
                         self))
               (lambda (s tok rest send)
                 (send tok
                       (lambda (next.in3)
                         (if (null? rest)
                             (input-continuation next.in3)
                             (s s (car rest)
                                (cdr rest)
                                next.in3))))))))))

    (define output/tok
      (lambda (tok next-input-tok expand/stream)
        (output-stream tok)
        (next-input-tok expand/stream)))

    (define dynamic/id
      (lambda (stok)
        (cond ((:PPTRIE.pptok.TIME__? (PP.static_lexeme.id stok))
               (dynamic-macro-object-time stok))
              ((:PPTRIE.pptok.DATE__? (PP.static_lexeme.id stok))
               (dynamic-macro-object-date stok))
              ((:PPTRIE.pptok.FILE__? (PP.static_lexeme.id stok))
               (dynamic-macro-object-file stok))
              ((:PPTRIE.pptok.LINE__? (PP.static_lexeme.id stok))
               (dynamic-macro-object-line stok))
              (else #f))))
    
    ((lambda (s) (s s))
     (lambda (self)
       (lambda (dtok next-input-tok)
         (define stok (PP.TOK.tok dtok))
         (define xenv (PP.TOK.env dtok))
         (define TRIEID (PP.static_lexeme.id stok))
         ;; (define DBG (stok 'TREE))
         ;; (__d "!!" (stok 'TREE))
         (cond ((pp-is-blue-set? dtok)
                (output/tok dtok next-input-tok (self self)))
               ((dynamic/id stok)
                => (lambda (dynamic/tok)
                     (output/tok 
                      (PP.TOK dynamic/tok (PP.TOK.blue_paint dtok) (PP.TOK.env dtok))
                      next-input-tok (self self))))
               ((and (PP.ID? stok)
                     (cpp-environment 'BOUND? xenv TRIEID)
                     (cpp-environment 'SEARCH xenv TRIEID))
                => (lambda (macrodef)
                     (define BLUE-SET (pp-blue-set-get dtok))
                     (ts macrodef xenv
                         (lambda (replacement-token-sequence)
                           "expand(subst(ts(T),fp(T),actuals,(BS(int)BS’)++{T},{})
                                   ++ TS'')"
                           ((lambda (s) (s s next-input-tok))
                            (lambda (s next-input0)
                              (next-input0
                               (lambda (maybe-oparen next-input1)
                                 "try and find a function call fun(..."
                                 (define XX/TOK (PP.TOK.tok maybe-oparen))
                                 (cond ((PP.NEWLINE? XX/TOK)
                                        "there may be newlines:  fun NL* ( ..."
                                        (s s next-input1))
                                       ((:PPTRIE.punct.OPEN_PAREN? (PP.static_lexeme.id XX/TOK))
                                        "found a promission of a function call"
                                        (function-call-try-collect-arguments
                                         (PP.MACRO_FUN.name macrodef)
                                         (PP.MACRO_FUN.param_count macrodef)
                                         next-input1 
                                         (lambda (arg* next-input2 closeparen)
                                           "found a function call."
                                           ;; (funcall:dbg arg* stok)
                                           (cpp-macro-substitution
                                            replacement-token-sequence
                                            (PP.MACRO_FUN.param_count macrodef)
                                            arg*
                                            ;; when close-paren is taken from continuation input this
                                            ;; will permit reevaluation of some funcall symbol
                                            (set-add (set-intersect (pp-blue-set-get closeparen) 
                                                                    BLUE-SET)
                                                     TRIEID)
                                            (substituted-stream next-input2 (self self))
                                            xenv))
                                         (lambda ()
                                           "panic -- cannot collect arguments"
                                           (stok 'TREE))))
                                       (else
                                        "no function call; output the tok-id and forget `maybe-oparen`"
                                        (output/tok dtok next-input-tok (self self)))))))))
                         (lambda (replacement-token-sequence)
                           "expand(subst(ts(T),{},{},BS++{T},{})
                                   ++ TS')"
                           (cpp-macro-substitution
                            replacement-token-sequence
                            '() '() (set-add BLUE-SET TRIEID)
                            (substituted-stream next-input-tok (self self))
                            xenv)))))
               (else
                (output/tok dtok next-input-tok (self self)))))))))

;;; #DEFINE
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define cpp-pdir-define
  (lambda (name I env return)
    (return (cpp-environment 'ADD env name I))))

(define cpp-pdir-undef
  (lambda (name env return)
    (return (cpp-environment 'REMOVE env name))))

;;; #INCLUDE
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define cpp-search-header
  (lambda (search-paths header s f)
    (define iter
      (lambda (search-paths)
        (define found-header?
          (lambda ()
            (let ((full-header
                   (and (cons? search-paths)
                        (build-path (car search-paths) header))))
              (and (or (file-exists? full-header)
                       (link-exists? full-header))
                   full-header))))
        (cond ((null? search-paths) (f))
              ((found-header?) => s)
              (else (iter (cdr search-paths))))))
    (iter search-paths)))

(define cpp-pdir-include-local
  (lambda (headername env close/stream tok/provider)
    (define local-dir ((cpp-path-stack 'GET)))
    (cpp-search-header
     (list local-dir) headername
     (lambda (h) (cpp-file h env close/stream tok/provider))
     (lambda ()  (cpp-pdir-include-system
             headername env close/stream tok/provider)))))

(define cpp-pdir-include-system
  (lambda (headername env close/stream tok/provider)
    (cpp-search-header
     SYSTEM-HEADERS-SEARCH-PATH headername
     (lambda (h) (cpp-file h env close/stream tok/provider))
     (lambda ()  (PANIC "cannot find header file" headername)))))

;;; CONDITIONAL GROUPS
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define cpp-pdir-conditional-notdef
  (lambda (id pointer else-branch-pointer env return)
    (let ((JMP (if (cpp-environment 'BOUND? env id)
                   (- pointer else-branch-pointer)
                   1)))
      (return JMP))))
(define cpp-pdir-conditional-def
  (lambda (id pointer else-branch-pointer env return)
    (let ((JMP (if (cpp-environment 'BOUND? env id)
                   1
                   (- pointer else-branch-pointer))))
      (return JMP))))
(define cpp-pdir-conditional-defined
  (PP.INTEGER_DEC 'definedop :PPTRIE.pptok.ONE  -1 -1 -1 -1 '_ "." 'a))
(define cpp-pdir-conditional-notdefined
  (PP.INTEGER_DEC 'definedop :PPTRIE.pptok.ZERO -1 -1 -1 -1 '_ "." 'a))
(define cpp-pdir-conditional
  (lambda (test defined_idx pointer else-branch-pointer env return)
    (define resolve-defined
      (lambda (test-expression)
        "first replace defined(id) with 0 or 1"
        (for-each (lambda (p)
                    (let ((defined (vector-ref test p)))
                      (let ((id (PP.DEFINED.id defined)))
                        (let ((bound? (cpp-environment 'BOUND? env id)))
                          (vector-set! test-expression p
                                       (if bound?
                                           cpp-pdir-conditional-defined
                                           cpp-pdir-conditional-notdefined))))))
                  defined_idx)
        test-expression))
    
    (define eval:stack
      '(()))
    
    (define eval:stack:reduce!
      (lambda ()
        (cpp-evaluate-expression
         (reverse (car eval:stack))
         (lambda (v)
           (set! eval:stack (cons v (cdr eval:stack)))))))
    
    (define update-test-expression
      (lambda (dtok)
        (define tok (PP.TOK.tok dtok))
        (define eval:stack++!
          (lambda (a)
            (set! eval:stack (cons (cons a (car eval:stack))
                                   (cdr eval:stack)))))
        (define eval:stack:new!
          (lambda ()
            (set! eval:stack (cons '() eval:stack))))
        (define eval:stack:close!
          (lambda ()
            (eval:stack:reduce!)
            (set! eval:stack
                  (cons (cons (car eval:stack)
                              (cadr eval:stack))
                        (cddr eval:stack)))))
        
        (case (tok 'CONSID)
          ('PP.INTEGER_DEC ((CTRIE 'VALUE.INT.DEC) (PP.INTEGER_DEC.id tok) 
                            (lambda (a _) (eval:stack++! a))))
          ('PP.INTEGER_HEX ((CTRIE 'VALUE.INT.HEX) (PP.INTEGER_HEX.id tok)
                            (lambda (a _) (eval:stack++! a))))
          ('PP.INTEGER_OCT ((CTRIE 'VALUE.INT.OCT) (PP.INTEGER_OCT.id tok)
                            (lambda (a _) (eval:stack++! a))))
          ('PP.INTEGER_BIN ((CTRIE 'VALUE.INT.BIN) (PP.INTEGER_BIN.id tok)
                            (lambda (a _) (eval:stack++! a))))
          ('PP.PUNCTUATOR (let* ((ID (PP.PUNCTUATOR.id tok))
                                 (OP (PP.OPERATOR ID)))
                            (case OP
                              ('OPEN-PAREN (eval:stack:new!))
                              ('CLOSE-PAREN (eval:stack:close!))
                              (else (eval:stack++! (cons OP ID))))))
          ('PP.ID         (eval:stack++! 0))
          ('PP.EOS        (eval:stack:reduce!)
                          (set! eval:stack (reverse eval:stack)))
          ('PP.CHARACTER  ((CTRIE 'VALUE.CHAR) (PP.CHARACTER.id tok)
                           (lambda (a _) (eval:stack++! a))))
          (else (error "todo.x" (tok 'TREE))))))
    
    (let ((test-expression (if (null? defined_idx)
                               test
                               (resolve-defined (vector-copy test)))))
      (cpp/eval test-expression env
                (lambda (.. __)
                  (cpp-evaluate-expression
                   eval:stack
                   (lambda (testval)
                     (let ((JMP (if (zero? testval)
                                    (- pointer else-branch-pointer)
                                    1)))
                       (return JMP)))))
                (cpp-macro-expand update-test-expression)))))

;;; NON CONDITIONAL DIRECTIVES
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define cpp-pdir-error
  (lambda (message)
    (__p "CPP ERROR -- ")
    (for-each (lambda (a)
                (pptrie.accumulator (PP.static_lexeme.id a)
                                    (lambda (m _)
                                      (__p
                                       (list->string
                                        (map integer->char m)))
                                      (__p " "))))
              message)
    (newline)
    'stop))

;;; #CPP EVALUATOR
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define cpp-environment
  (lambda (m e k . args)
    (case m
      ('BOUND?  (hash-has-key? e k))
      ('SEARCH  (hash-ref e k #f))
      ('ADD     (hash-set e k (car args)))
      ('REMOVE  (hash-remove e k))
      (else (error "env:" m)))))
(define cpp:env-dbg
  (lambda (env)
    (__d ".E")
    (hash-for-each env
                   (lambda (k v)
                     (pptrie.accumulator
                      k
                      (lambda (X Y)
                        (__d "X" (list->string
                                  (map integer->char X)))))))))

(define cpp/eval
  (lambda (ppcode env-init close/stream tok/provider/init)
    "~~ interpret a vector of instructions of pp-tokens and pp-directives
     ~~ emit pp-tokens via tok/provider
     ~~ call close/stream at the end of the stream.
     ~~ after a token is emitted it may receive a new tok/provider
        to continue emitting on
     ~~ in case the current instruction is a directive instruction it does not 
        output any token, but only interpret the directive."    
    (define cpp/eval
      (lambda (pointer env tok/provider)
        (define I (vector-ref ppcode pointer))
        (if (PP.directive? I)
            (case (I 'CONSID)
              ('PP.MACRO_FUN
               (cpp-pdir-define
                (PP.MACRO_FUN.name I) I env
                (lambda (env1)
                  (cpp/eval (add1 pointer) env1 tok/provider))))
              ('PP.MACRO_OBJ
               (cpp-pdir-define
                (PP.MACRO_OBJ.name I) I env
                (lambda (env1)
                  (cpp/eval (add1 pointer) env1 tok/provider))))
              ('PP.UNDEF
               (cpp-pdir-undef
                (PP.UNDEF.id I)
                env
                (lambda (env1)
                  (cpp/eval (add1 pointer) env1 tok/provider))))
              ('PP.INCLUDE_LOCAL
               (cpp-pdir-include-local
                (pptrie.accumulator
                 (PP.INCLUDE_LOCAL.header_id I)
                 (lambda (acc _) (list->string (map integer->char acc))))
                env
                (lambda (env1 next/tok/provider)
                  (cpp/eval (add1 pointer) env1 next/tok/provider))
                tok/provider))
              ('PP.INCLUDE_SYSTEM
               (cpp-pdir-include-system
                (pptrie.accumulator
                 (PP.INCLUDE_SYSTEM.header_id I)
                 (lambda (acc _) (list->string (map integer->char acc))))
                env
                (lambda (env1 next/tok/provider)
                  (cpp/eval (add1 pointer) env1 next/tok/provider))
                tok/provider))
              ('PP.IF
               (cpp-pdir-conditional
                (PP.IF.test I)
                (PP.IF.defined_idx I)
                (PP.IF.pointer I)
                (PP.IF.else_branch_pointer I)
                env
                (lambda (JMP)
                  (cpp/eval (+ JMP pointer) env tok/provider))))
              ('PP.IFNDEF
               (cpp-pdir-conditional-notdef
                (PP.IFNDEF.id I)
                (PP.IFNDEF.pointer I)
                (PP.IFNDEF.else_branch_pointer I)
                env
                (lambda (JMP)
                  (cpp/eval (+ JMP pointer) env tok/provider))))
              ('PP.IFDEF
               (cpp-pdir-conditional-def
                (PP.IFDEF.id I)
                (PP.IFDEF.pointer I)
                (PP.IFDEF.else_branch_pointer I)
                env
                (lambda (JMP)
                  (cpp/eval (+ JMP pointer) env tok/provider))))
              ('PP.ELSE
               (cpp/eval (add1 pointer) env tok/provider))
              ('PP.ENDIF
               (cpp/eval (add1 pointer) env tok/provider))
              ('PP.JMP
               (cpp/eval (+ (PP.JMP.pointer_offset I)
                            pointer)
                         env tok/provider))
              ('PP.ERROR
               (cpp-pdir-error (PP.ERROR.message I)))
              (else
               (error "to implement --" (I 'TREE))))
            (tok/provider
             (PP.TOK I (cpp-blue-set-empty) env)
             (lambda (next/tok/provider)
               (if (PP.EOS? I)
                   (close/stream env next/tok/provider)
                   (cpp/eval (add1 pointer)
                             env
                             next/tok/provider)))))))
    (cpp/eval 0 env-init tok/provider/init)))

;;; CPP FILE
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define cpp-path-stack
  (let ((dirs  '())
        (files '()))
    (define push
      (lambda (d0)
        (define dir (simplify-path (path-only d0)))
        (define file (file-name-from-path d0))
        ;; (__d "PUSH DIR" dir "=>" file)
        (set! dirs (cons dir dirs))
        (set! files (cons file files))))
    (define pop
      (lambda ()
        ;; (__d "POP DIR" (car dirs) "=>" (car files))
        (set! dirs (cdr dirs))
        (set! files (cdr files))))
    (define gettopdir
      (lambda ()
        (car dirs)))
    (define gettopfile
      (lambda ()
        (if (cons? files) (car files) '.eos)))
    (lambda (m)
      (case m
        ('PUSH push)
        ('POP  pop)
        ('GET  gettopdir)
        ('GETF gettopfile)))))

(define cpp-file
  (lambda (FILE env close/file macro-expand-instance)
    ;; (__d ".CPP/FILE/IN" FILE)
    ((cpp-path-stack 'PUSH) FILE)
    (preprocess123
     FILE
     (lambda (pp.lexemes*)
       (cpp/eval pp.lexemes* env
                 (lambda (env1 tok/provider)
                   ((cpp-path-stack 'POP))
                   ;; (__d ".CPP/FILE/RETURNTO" ((cpp-path-stack 'GETF)))
                   (close/file env1 tok/provider))
                 macro-expand-instance)))))

;;; PREPROCESSING STEP 4
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define cpp4
  (lambda (file intitial-cpp-environment output return)
    (cpp-file file intitial-cpp-environment
              (lambda (env macro-expand-instance)
                "close file"
                ;; (__d "." (preprocessor-trie 'count))
                ;; (__d "\nfinish")
                (return (c/lexemes/output 'close)))
              (cpp-macro-expand output))))




