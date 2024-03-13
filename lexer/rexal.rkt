;; -*- mode:scheme ; buffer-read-only:nil -*-

;;; A compiler from the language of the regular expressions to a
;;; sublanguage of combinators of the regular expressions algebra.

(define CONCAT
  `(,(RE.POP 'Y)
    ,(RE.POP 'X)
    ,(RE.EDGE 'X 'Y)
    ,(RE.PUSH 'X)))
(define NEW
  (lambda (char)
    `(,(RE.NEW 'X)
      ,(RE.SET 'X char)
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

(define re/compile
  (lambda (e)
    (re/alternative e
                    (lambda (re rest)
                      (or (null? rest)
                          (error "bad regular expression" e))
                      (re/processor (flatten re))))))

(define re/stream/expect
  (lambda (stream c)
    (or (and (cons? stream)
             (eq? (car stream) c))
        (PANIC (~a "bad rex ; expected " c " -- " stream))
        (exit 1))))

(define re/alternative
  (lambda (e y)
    ((lambda (u) (u u e y))
     (lambda (u s co)
       (re/concat s
                  (lambda (re1 rest1)
                    (cond ((null? rest1)
                           (co re1 rest1))
                          ((char=? (car rest1) #\|)
                           (u u (cdr rest1)
                              (lambda (re2 rest2)
                                (co (list re1 re2 UNION)
                                    rest2))))
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
           (re/kleene s
                      (lambda (t rest)
                        (u u rest
                           (if (null? acc)
                               t
                               (list t acc CONCAT))))))))))

(define re/kleene
  (lambda (e y)
    (re/term e
             (lambda (term rest)
               (if (and (pair? rest)
                        (char=? #\* (car rest)))
                   (y (list term KLEENE) (cdr rest))
                   (y term rest))))))

(define re/term
  (lambda (e y)
    (cond ((null? e)
           (y 'void '()))
          ((char=? #\[ (car e))
           (re/complex/char e
                            (lambda (cc rest)
                              (y cc rest))))
          ((char=? #\( (car e))
           (re/alternative (cdr e)
                           (lambda (alt rest)
                             (re/stream/expect rest #\))
                             (y alt (cdr rest)))))
          (else
           (re/new/char e
                        (lambda (c rest)
                          (y c rest)))))))

(define re/complex/char
  (lambda (e y)
    (re/stream/expect e #\[)
    (define negation?
      (char=? #\^ (cadr e)))
    (define chset (make-character-set negation?))
    (define iter
      (lambda (q first?)
        (cond ((and (not first?)
                    (char=? #\] (car q)))
               (cdr q))
              ((or (null? q)
                   (null? (cdr q)))
               (PANIC "regexp complex-char error " e))
              ((eq? (cadr q) #\-)
               (chset-set-val-in-interval
                chset
                (make-interval (car q) (caddr q))
                (not negation?))
               (iter (cdddr q) #f))
              (else
               (chset-set chset
                          (car q)
                          (not negation?))
               (iter (cdr q) #f))))) 
    (let ((r (iter ((if negation? cddr cdr) e)
                   #t)))
      (y (NEW (cons 'UNION (character-set-filter chset)))
         r))))

(define re/new/char
  (lambda (e y)
    (y (NEW (car e))
       (cdr e))))

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
               ('RE.SET
                (define reg (RE.SET.r I))
                (define v (RE.SET.x I))
                (case reg
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.EDGE
                (define regA (RE.EDGE.r I))
                (define regB (RE.EDGE.p I))
                (define v0 (case regA ('X x) ('Y y)))
                (define v1 (case regB ('X x) ('Y y)))
                (define v `(CONCAT ,v1 ,v0))
                (case regA
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.ALTERNATIVE
                (define regA (RE.ALTERNATIVE.r I))
                (define regB (RE.ALTERNATIVE.p I))
                (define v0 (case regA ('X x) ('Y y)))
                (define v1 (case regB ('X x) ('Y y)))
                (define v `(UNION ,v1 ,v0))
                (case regA
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               ('RE.KLEENE
                (define reg (RE.KLEENE.r I))
                (define v0 (case reg ('X x) ('Y y)))
                (define v `(REPEAT 0 INF ,v0))
                (case reg
                  ('X (u u (cdr code) stack v y))
                  ('Y (u u (cdr code) stack x v))))
               (else
                (error "regular expression processor:"
                       'ALERT!!! (I 'TREE))))))))))

