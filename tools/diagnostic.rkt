;; -*- mode:scheme ; buffer-read-only:nil -*-

(define ERROR-COUNT 0)
;; (define WARNING-COUNT 0)

(define PANIC
  (lambda args
    (set! ERROR-COUNT (add1 ERROR-COUNT))
    (apply __stderr (cons "PANIC:" args))
    'panic))

(define WARNING
  (lambda args
    ;; (set! WARNING-COUNT (add1 WARNING-COUNT))
    (apply __stderr (cons "WARNING:" args))
    'warning))

