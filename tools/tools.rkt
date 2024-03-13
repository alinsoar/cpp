;; -*- mode:scheme ; buffer-read-only:nil -*-

;;; COUNTERS
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define counter
  (let ((counters (make-hash)))
    "Donec eris felix, multos numerabis amicos."
    (define (get id)
      (hash-ref! counters id (lambda () 0)))
    (define (1+ id)
      (hash-set! counters id (add1 (get id)))
      (get id))
    (define (reset id n)
      (hash-set! counters id n))
    (lambda (perm)
      (perm get 1+ reset))))
(define counter/get
  (lambda (id)
    (counter (lambda (get _ __) (get id)))))
(define 1+
  (counter (lambda (_ 1+ __) 1+)))
(define counter/reset
  (counter (lambda (_ __ r) r)))

;;; PROFILER
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define profiler
  (let ((profilers (make-hash))
        (interval (make-hash)))
    (define (get ID)
      (hash-ref! profilers ID '()))
    (define (start ID)
      (hash-set! interval ID (current-inexact-milliseconds)))
    (define (end ID)
      (define diff (- (current-inexact-milliseconds)
                      (hash-ref interval ID
                                (lambda () (error "timer not started")))))
      (hash-set! profilers ID (cons diff (get ID)))
      (hash-remove! interval ID))
    (define (reset ID)
      (hash-remove! interval ID)
      (hash-remove! profilers ID))
    (lambda (ID m)
      ((case m
         ('GET   get)
         ('START start)
         ('END   end)
         ('RESET reset))
       ID))))

;;; UNION FIND
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define union/find
  (lambda (size)
    
    (define links
      (vector-map (lambda (x) x) (list->vector (range size)))) 
    
    (define root
      (lambda (node)
        ((lambda (s) (s s node))
         (lambda (s node)
           (let ((parent (vector-ref links node)))
             (if (= node parent)
                 node
                 (begin
                   (vector-set! links node parent)
                   (s s parent))))))))
    
    (define unite
      (lambda (a b)
        (define top.a (root a))
        (define top.b (root b))
        (vector-set! links top.a top.b)
        'ok))
    
    (define find
      (lambda (a b)
        (= (root a) (root b))))
    
    (define group
      (lambda ()
        (map (lambda (x) (vector-set! links x (root x)))
             (range size))
        links))
    
    (lambda (m)
      (case m
        ('UNITE    unite)
        ('FIND     find)
        ('GROUP    group)))))

;;; CHARACTERS
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define ESCAPE         (char->integer #\\))
(define BS             (char->integer #\\))
(define NL             (char->integer #\newline))
(define SPACE          (char->integer #\space))
(define TAB            (char->integer #\tab))
(define LESSTHAN       (char->integer #\<))
(define GREATERTHAN    (char->integer #\>))
(define PARAGRAPH      (char->integer #\"))
(define WIDE-PREFIX/L  (char->integer #\L))
(define UNICODE/u4     (char->integer #\u))
(define UNICODE/U8     (char->integer #\U))
(define HEXAx1         (char->integer #\x))
(define HEXAX2         (char->integer #\X))
(define DOT            (char->integer #\.))

;;;
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define whitespace? (lambda (x) (or (= x TAB) (= x SPACE))))
