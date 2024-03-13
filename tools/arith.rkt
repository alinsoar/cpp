;; -*- mode:scheme ; buffer-read-only:nil -*-

;;;            PRIME NUMBER GENERATOR BASED ON THE SIEVE OF ERATOSTHENES
;;; ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

(define integers-step2-from
  (lambda (from)
    ((lambda (s) (s s from))
     (lambda (s n)
       (cons n (lambda ()
                 (s s (+ 2 n))))))))

(define odd-integers
  (integers-step2-from 3))

(define divisible?
  (lambda (n m) (zero? (remainder n m))))

(define s/filter
  (lambda (pred s)
    (if (pred (car s))
        (cons (car s)
              (lambda ()
                (s/filter pred
                          ((cdr s)))))
        (s/filter pred
                  ((cdr s))))))

(define eratosthenes
  (lambda (n stream)
    "the sieve of eratosthenes"
    (cons
     n
     (lambda ()
       (eratosthenes (car stream)
                     (s/filter
                      (lambda (x) (not (divisible? x (car stream))))
                      ((cdr stream))))))))

(define eratosthenes-faster
  (lambda (Q stream)
    "the sieve of eratosthenes"
    (cons
     (car stream)
     (lambda ()
       (let ((next-prime (car stream)))
         (eratosthenes-faster
          (* next-prime Q)
          (s/filter
           (lambda (x) (= 1 (gcd x Q)))
           (integers-step2-from
            (+ 2 (car stream))))))))))
