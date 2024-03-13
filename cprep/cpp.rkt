;; -*- mode:scheme ; buffer-read-only:nil -*-

(define INTITIAL-CPP-ENVIRONMENT
  (make-immutable-hash))

(define DEFAULT-VALUES-FILE
  "./cprep/include/defaults.h")

(define output-stream1
  (lambda (dtok)
    (define tok (PP.TOK.tok dtok))
    (define TRIEID (PP.static_lexeme.id tok))
    (__d "\t\t\t> > >"
         (~a "`" (if (PP.NEWLINE? tok)
                     "\\n"
                     (pp:dbg tok)) "`")
         (tok 'TREE))))

(define output-stream2
  (lambda (dtok)
    (define tok (PP.TOK.tok dtok))
    (__p (pp:dbg tok))))

(define output-cpp4
  ;; One of:
  ;; ~~ output-stream1
  ;; ~~ output-stream2
  ;; ~~ output-stream-567
  output-stream-567
  )

(define cpp
  (lambda (FILE return)
    (__d ".CPP" FILE)
    (cpp-file
     DEFAULT-VALUES-FILE INTITIAL-CPP-ENVIRONMENT
     (lambda (intitial-cpp-environment _)
       ;; (cpp:env-dbg env)
       (cpp4 FILE intitial-cpp-environment output-cpp4
             return))
     (cpp-macro-expand (lambda _ _)))))

