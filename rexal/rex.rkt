;; -*- mode:scheme ; buffer-read-only:t -*-

;;; A compiler from the language of the regular expressions to a
;;; language of combinators of the regular expressions algebra.

(define CONCAT
  `(,(RE.POP 'Y)
    ,(RE.POP 'X)
    ,(RE.EDGE 'X 'Y)
    ,(RE.PUSH 'X)))
(define NEW
  (lambda (char)
    `(,(RE.NEW 'X)
      ,(RE.ASSIGN 'X char)
      ,(RE.PUSH 'X))))
(define ANY
  `(,(RE.NEW 'X)
    ,(RE.ANY 'X)
    ,(RE.PUSH 'X)))
(define INTERVAL
  (lambda (from to)
    `(,(RE.NEW 'X)
      ,(RE.ASSIGN 'X from)
      ,(RE.NEW 'Y)
      ,(RE.ASSIGN 'Y to)
      ,(RE.INTERVAL 'X 'Y)
      ,(RE.PUSH 'X))))
(define UNION
  `(,(RE.POP 'Y)
    ,(RE.POP 'X)
    ,(RE.ALTERNATIVE 'X 'Y)
    ,(RE.PUSH 'X)))
(define KLEENE
  `(,(RE.POP 'X)
    ,(RE.KLEENE 'X)
    ,(RE.PUSH 'X)))
(define OPTIONAL
  `(,(RE.POP 'X)
    ,(RE.OPT 'X)
    ,(RE.PUSH 'X)))
(define COMPLEMENT/CHAR
  `(,(RE.POP 'X)
    ,(RE.COMPLEMENT 'X)
    ,(RE.PUSH 'X)))
(define BEGINNING/OF/LINE
  `(,(RE.POP 'X)
    ,(RE.BOL 'X)
    ,(RE.PUSH 'X)))

(define re/stream/expect
  (lambda (stream c)
    (or (and (cons? stream)
             (eq? (car stream) c))
        (PANIC (~a "bad rex ; expected " c " -- " stream))
        (exit 1))))

(define re/compile
  (lambda (e)
    (re/beginning/of/line
     e
     (lambda (re rest)
       (or (null? rest)
           (error "bad regular expression" e))
       (and (null? re)
            (error "bad regular expression" e))
       (re/processor (flatten re))))))

(define re/beginning/of/line
  (lambda (e y)
    (define bol? (char=? (car e) #\^))
    (and bol?
         (null? (cdr e))
         (error "beginning-of-line operator: expected expression" e))
    (re/alternative (if bol? (cdr e) e)
                    (lambda (re rest)
                      (if bol?
                          (y (list re BEGINNING/OF/LINE)
                             rest)
                          (y re rest))))))

(define re/alternative
  (lambda (e y)
    ((lambda (u) (u u e y))
     (lambda (u s co)
       (re/concat
        s
        (lambda (re1 rest1)
          (cond ((null? rest1)
                 (co re1 rest1))
                ((char=? (car rest1) #\|) 
                 (u u (cdr rest1)
                    (lambda (re2 rest2) 
                      (cond ((and (null? re1) (null? re2))
                             (co '() rest2))
                            ((null? re1)
                             (co  (list re2 OPTIONAL) rest2))
                            ((null? re2)
                             (co  (list re1 OPTIONAL) rest2))
                            (else
                             (co (list re1 re2 UNION) rest2))))))
                (else
                 ;; invalid regex
                 (co re1 rest1)))))))))

(define re/concat
  (lambda (e y)
    ((lambda (u) (u u e '()))
     (lambda (u s acc)
       (if (or (null? s)
               (memq (car s) '(#\| #\))))
           (y acc s)
           (re/kleene+optional
            s
            (lambda (t rest)
              (u u rest
                 (if (null? acc)
                     t
                     (list t acc CONCAT))))))))))

(define re/kleene+optional
  (lambda (e y)
    (re/term e
             (lambda (term rest)
               ((lambda (s) (s s rest term))
                (lambda (s rest acc)
                  (if (and (pair? rest)
                           (char=? #\* (car rest)))
                      (s s (cdr rest) (list acc KLEENE))
                      (if (and (pair? rest)
                               (char=? #\? (car rest)))
                          (s s (cdr rest) (list acc OPTIONAL))
                          (y acc rest)))))))))

(define re/term
  (lambda (e y)
    (cond ((null? e)
           (y 'void '()))
          ((char=? #\[ (car e))
           (re/complex/char e y))
          ((char=? #\( (car e))
           (re/alternative
            (cdr e)
            (lambda (alt rest)
              (re/stream/expect rest #\))
              (y alt (cdr rest)))))
          (else
           (re/new/char e y)))))

(define re/complex/char
  (lambda (e y)
    (re/stream/expect e #\[)
    (define negation? (char=? #\^ (cadr e)))
    (define iter
      (lambda (q first? col)
        (cond ((null? q)
               (error "complex-char" e))
              ((and (not first?)
                    (char=? #\] (car q)))
               (col '() (cdr q)))
              ((null? (cdr q))
               (error "regexp complex-char error " e))
              ((eq? (cadr q) #\-)
               (or (cons? (cddr q))
                   (error "regexp complex-char interval error "
                          e))
               (define from (car q))
               (define to (add1 (char->integer (caddr q))))
               (define I (INTERVAL from to))
               (iter (cdddr q) #f
                     (lambda (chset r)
                       (col (if (null? chset)
                                I
                                (list I chset UNION))
                            r))))
              (else
               (iter (cdr q) #f
                     (lambda (chset r)
                       (define x (NEW (car q)))
                       (col (if (null? chset)
                                x
                                (list x chset UNION))
                            r)))))))
    (iter ((if negation? cddr cdr) e)
          #t
          (lambda (chset r)
            (define s chset)
            (y (if negation?
                   (cons s COMPLEMENT/CHAR)
                   s )
               r)))))

(define re/new/char
  (lambda (e y)
    (if (char=? #\. (car e))
        (y ANY (cdr e))
        (y (NEW (car e)) (cdr e)))))

(define re/processor
  (lambda (code)
    ((lambda (u) (u u code '() '$ '$))
     (lambda (u code stack x y)
       (if (null? code)
           (car stack)
           (let ((I (car code)))
             (case (I 'CONSID)
               ('RE.POP
                (define reg (RE.POP.r I))
                (case reg
                  ('X (u u (cdr code) (cdr stack) (car stack) y))
                  ('Y (u u (cdr code) (cdr stack) x (car stack)))))
               ('RE.PUSH
                (define reg (RE.PUSH.r I))
                (case reg
                  ('X (u u (cdr code) (cons x stack) x y))
                  ('Y (u u (cdr code) (cons y stack) x y))))
               ('RE.NEW
                (define reg (RE.NEW.r I))
                (case reg
                  ('X (u u (cdr code) stack '$ y))
                  ('Y (u u (cdr code) stack x '$))))
               ('RE.ASSIGN
                (define reg (RE.ASSIGN.r I))
                (define v (RE.ASSIGN.x I))
                (case reg
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.EDGE
                (define regA (RE.EDGE.r I))
                (define regB (RE.EDGE.p I))
                (define v0 (case regA ('X x) ('Y y)))
                (define v1 (case regB ('X x) ('Y y)))
                (define v
                  (cond ((and (OP.CONCAT? v1) (OP.CONCAT? v0))
                         (append v1 (cdr v0)))
                        ((OP.CONCAT? v1)
                         (append v1 (list v0)))
                        ((OP.CONCAT? v0)
                         (append (list v1) (cdr v0)))
                        (else
                         `(CONCAT ,v1 ,v0))))
                (case regA
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.ALTERNATIVE
                (define regA (RE.ALTERNATIVE.r I))
                (define regB (RE.ALTERNATIVE.p I))
                (define v0 (case regA ('X x) ('Y y)))
                (define v1 (case regB ('X x) ('Y y)))
                (define v
                  (cond ((and (OP.UNION? v1)
                              (OP.UNION? v0))
                         (append v1 (cdr v0)))
                        ((OP.UNION? v1)
                         (append v1 (list v0)))
                        ((OP.UNION? v0)
                         (append (list v1) (cdr v0)))
                        (else
                         `(UNION ,v1 ,v0))))
                (case regA
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.KLEENE
                (define reg (RE.KLEENE.r I))
                (define v0 (case reg ('X x) ('Y y)))
                (define v (OP.REPEAT 0 'INF v0))
                (case reg
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.INTERVAL
                (define regA (RE.INTERVAL.r I))
                (define regB (RE.INTERVAL.p I))
                (define v0 (case regA ('X x) ('Y y)))
                (define v1 (case regB ('X x) ('Y y)))
                (define v `(INTERVAL ,v0 ,v1))
                (case regA
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.COMPLEMENT
                (define reg (RE.COMPLEMENT.r I))
                (define v0 (case reg ('X x) ('Y y)))
                (define v (OP.COMPLEMENT
                           `(UNION
                             (UNION)
                             (CONCAT ,v0 ,(OP.REPEAT 0 'INF OP.ANY)))))
                (case reg
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.BOL
                (define reg (RE.BOL.r I))
                (define v0 (case reg ('X x) ('Y y)))
                (define v `(BOL ,v0))
                (case reg
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.ANY
                (define reg (RE.ANY.r I))
                (define v0 (case reg ('X x) ('Y y)))
                (define v OP.ANY)
                (case reg
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.OPT
                (define reg (RE.OPT.r I))
                (define v0 (case reg ('X x) ('Y y)))
                (define v
                  (if (OP.OPT? v0) v0 (OP.OPT v0)))
                (case reg
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               (else
                (error "regular expression processor:"
                       'ALERT!!! (I 'TREE))))))))))

