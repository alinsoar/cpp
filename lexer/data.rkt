;; -*- mode:scheme ; buffer-read-only:nil -*-

(define ALPHABET-SIZE 256)
(define operator car)
(define operands cdr)
(define tagged?
  (lambda (tag l . op)
    (and (pair? l)
         ((if (null? op) eq? (car op))
          (operator l) tag))))

(define interval?
  (lambda (e)
    (tagged? 'INTERVAL e)))
(define make-interval
  (lambda (< >)
    (let ((min (if (char? <) < (integer->char <)))
          (max (if (char? >) > (integer->char >))))
      (or (char<? min max)
          (begin
            (PANIC "invalid interval --" < >)
            (exit 1)))
      (cons 'INTERVAL (cons min max)))))
(define interval-min
  (lambda (int)
    (if (char? (cadr int))
        (char->integer (cadr int))
        (cadr int))))
(define interval-max
  (lambda (int)
    (if (char? (cadr int))
        (char->integer (cddr int))
        (cddr int))))
(define interval-for-each
  (lambda (int f)
    (for-each (lambda (a)
                (f (integer->char a)))
              (range (interval-min int)
                     (add1 (interval-max int))))))
(define interval-for-each/0
  (lambda (i/rands f)
    (for-each (lambda (a)
                (f (integer->char a)))
              (range (char->integer (car i/rands))
                     (add1 (char->integer (cdr i/rands)))))))
(define interval-for-each/1
  (lambda (i/rands f)
    (for-each f (range (car i/rands) (add1 (cdr i/rands))))))

(define interval->range
  (lambda (i)
    (range (interval-min i)
           (add1 (interval-max i)))))
(define char-in-interval?
  (lambda (i c)
    (and
     (<= (char->integer c) (interval-max i))
     (>= (char->integer c) (interval-min i)))))
(define in-interval?
  (lambda (i c)
    (and
     (<= c (interval-max i))
     (>= c (interval-min i)))))
(define interval->vector
  (lambda (i)
    (build-vector ALPHABET-SIZE
                  (lambda (x)
                    (in-interval? i x)))))

(define s->v
  (lambda (s)
    (let ((w (make-vector ALPHABET-SIZE #f)))
      (set-map
       s
       (lambda (x)
         (vector-set! w x #t)))
      w)))

(define v->s
  (lambda (v)
    (list->set
     (filter
      (λ (x) x)
      (map
       (lambda (x i)
         (and x i))
       (vector->list v)
       (range ALPHABET-SIZE))))))

;;; CHARACTER SETS

(define make-character-set
  (lambda (init)
    (make-vector ALPHABET-SIZE init)))

(define chset-set
  (lambda (cs pos v)
    (let ((pos (if (char? pos)
                   (char->integer pos)
                   pos)))
      (vector-set! cs pos v))))

(define chset-set-val-in-interval
  (lambda (cs int val)
    (interval-for-each
     int
     (lambda (i)
       (chset-set cs i val)))))

(define character-set-filter
  (lambda (cs)
    (map
     car
     (filter
      cdr
      (map cons
           (range ALPHABET-SIZE)
           (vector->list cs))))))
