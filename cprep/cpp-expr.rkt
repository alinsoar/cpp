;; -*- mode:scheme ; buffer-read-only:nil -*-

(define __unary-expression
  (lambda (a s f)
    (cond ((null? a) (f))
          ((number? (car a))
           (s (car a) (cdr a)))
          ((unary-operator (car a))
           => (lambda (op)
                (__unary-expression
                 (cdr a)
                 (lambda (v rest)
                   (s (op v) rest))
                 (lambda ()
                   (PANIC "cpp-expr: expected expression after unary operator"
                          (car a))))))
          (else
           (error "Unary" a)))))

(define unary-operator
  (lambda (op)
    (and (cons? op)
         (case (car op)
           ('LOGNOT   (lambda (rand) (if (zero? rand) 1 0)))
           ('SUBTRACT (lambda (rand) (- rand)))
           (else #f)))))

(define cpp/binop/value
  (lambda (rator left/rand right/rand ret)
    ;; (__d "--"  left/rand (~a "`" rator "`") right/rand)
    (case rator
      ('LOGAND     (ret (if (or  (zero? left/rand) (zero? right/rand)) 0 1)))
      ('LOGOR      (ret (if (and (zero? left/rand) (zero? right/rand)) 0 1)))
      ('GEQ        (ret (if (>= left/rand right/rand) 1 0)))
      ('LEQ        (ret (if (<= left/rand right/rand) 1 0)))
      ('EQ         (ret (if (=  left/rand right/rand) 1 0)))
      ('NEQ        (ret (if (=  left/rand right/rand) 0 1)))
      ('GT         (ret (if (>  left/rand right/rand) 1 0)))
      ('LT         (ret (if (<  left/rand right/rand) 1 0)))
      ('SUBTRACT   (ret (- left/rand right/rand)))
      ('ADDITION   (ret (+ left/rand right/rand)))
      ('SHL        (ret (arithmetic-shift left/rand right/rand)))
      ('SHR        (ret (arithmetic-shift left/rand (- right/rand))))
      (else (begin (PANIC "cpp/value" rator)
                   (error " will stop."))))))

(define condval
  (lambda (R e ret)
    (CPP/BINOP
     e
     __conditional-expression
     (lambda (e1 rest1)
       (if (and (cons? rest1)
                (cons? (car rest1))
                (:PPTRIE.punct.COLON? (cdar rest1)))
           (CPP/BINOP (cdr rest1)
                      __conditional-expression
                      (lambda (e2 rest2)
                        (ret (if (zero? R) e2 e1)
                             rest2))
                      (lambda () (PANIC "cpp-expr: expected expression4 after conditional" rest1)))
           (PANIC "cpp-expr: expected separator after conditional expression" e)))
     (lambda () (PANIC "cpp-expr: expected expression2 after conditional" e)))))

(define CPP/BINOP
  (lambda (e RIGHT.TYPE s f)
    "try to parse an expression of type
      left-expr OPERATOR right-expr
      or
      expression
      where OPERATOR has a type equal or of higter priority than RIGHT.TYPE
    "
    (define iter
      (lambda (REG rest)
        (define RATOR (and (cons? rest) (car rest)))
        (or (cons? RATOR)
            (null? rest)
            (begin
              (PANIC "expect an operator" rest)
              (exit 1)))
        (cond ((null? rest)
               (s REG '()))
              ((:PPTRIE.punct.QUESTION? (cdr RATOR))
               "a special form of type"
               (condval REG (cdr rest) s))
              (else
               ;; detect right type for the current ope`RATOR` that
               ;; has higter priority than RIGHT.TYPE.
               (RIGHT.TYPE
                (cdr RATOR)
                (lambda (parent/type)
                  "find an expression for the right type."
                  (CPP/BINOP (cdr rest)
                             parent/type
                             (lambda (right/rand rest2)
                               ;; left-accumulate the result in REG
                               ;; and try again the same operator of
                               ;; RIGHT.TYPE.
                               (cpp/binop/value (car RATOR)
                                                REG right/rand
                                                (lambda (val)
                                                  (iter val rest2))))
                             (lambda ()
                               (PANIC
                                "cpp-expr: expected operand after"
                                RATOR ";" rest))))
                (lambda ()
                  (s REG rest)))))))
    ;; start with the rightmost type -- atoms and unary operators
    (__unary-expression e iter f)))

(define CPP-LEFT-ASSOCIATIVE-NONTERMINAL
  (lambda (RIGHT-TYPE . CONSTRUCTORS)
    ((lambda (s) (s s))
     (lambda (s)
       (λ (op succ fail)
         "given a constructor operator OP, find its righthand type"
         ((λ (u) (u u CONSTRUCTORS))
          (lambda (u l)
            (cond ((null? l)
                   (RIGHT-TYPE op succ fail))
                  (((car l) op)
                   (succ RIGHT-TYPE))
                  (else
                   (u u (cdr l)))))))))))

(define __multiplicative-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   (lambda (_ s f) (f))
   :PPTRIE.punct.ASTERISK?
   :PPTRIE.punct.DIVISION?
   :PPTRIE.punct.REMAINDER? ))

(define __additive-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __multiplicative-expression
   :PPTRIE.punct.ADDITION?
   :PPTRIE.punct.SUBTRACTION? ))

(define __shift-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __additive-expression
   :PPTRIE.punct.SHIFT_LEFT?
   :PPTRIE.punct.SHIFT_RIGHT? ))

(define __relational-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __shift-expression
   :PPTRIE.punct.LESS_THAN?       
   :PPTRIE.punct.GREATER_THAN?    
   :PPTRIE.punct.LESS_OR_EQUAL?   
   :PPTRIE.punct.GREATER_OR_EQUAL? ))

(define __equality-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __relational-expression
   :PPTRIE.punct.DIFFERENT?
   :PPTRIE.punct.EQUAL? ))

(define __AND-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __equality-expression
   :PPTRIE.punct.AND? ))

(define __exclusive-OR-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __AND-expression
   :PPTRIE.punct.EXCLUSIVE_OR? ))

(define __inclusive-OR-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __exclusive-OR-expression
   :PPTRIE.punct.INCLUSIVE_OR? ))

(define __logical-AND-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __inclusive-OR-expression
   :PPTRIE.punct.LOGAND? ))

(define __logical-OR-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __logical-AND-expression
   :PPTRIE.punct.LOGOR? ))

(define __conditional-expression
  (CPP-LEFT-ASSOCIATIVE-NONTERMINAL
   __logical-OR-expression
   :PPTRIE.punct.QUESTION? ))

(define cpp-evaluate-expression
  (lambda (expr ret)
    ;; (__w '... expr)
    (CPP/BINOP
     expr
     __conditional-expression
     (lambda (e r)
       (if (null? r)
           (ret e)
           (PANIC "cpp-expr: expected operator" expr)))
     (lambda ()
       (PANIC "cpp-expr: #if expects an expression" expr)))))

