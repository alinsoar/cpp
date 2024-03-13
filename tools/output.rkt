;; -*- mode:scheme ; buffer-read-only:nil -*-

(define __fcol
  (lambda (W A P . args)
    "format column"
    (apply ~a
           args
           #:width      W
           #:align      A
           #:pad-string P)))

(define (__q x y)
  (lambda m
    (and (eq? x y)
         (apply __p (append m '("\n")))
         (void))))

(define __stderr
  (lambda args
    (display (foldr (lambda (a b) (~a a " " b)) "" args)
             (current-error-port))
    (newline)))

  ;; generic display on some port
(define __display_generic
  (lambda (port OUT)
    (lambda args
      (define (iter a co)
        (if (null? a)
            (co (lambda ()
                  (newline port)
                  (flush-output port)))
            (iter (cdr a)
                  (lambda (x)
                    (co (lambda ()
                          (display " " port)
                          (OUT (car a) port)
                          (x)))))))
      (void (iter args (lambda (x) (x) ) ) ) ) ) )

  ;; generic display on stderr
(define __e (__display_generic (current-error-port) display))

  ;; generic display on stdout
(define __d (__display_generic (current-output-port) display))
(define __w (__display_generic (current-output-port) write))

(define (__p . args)
  (void (map display args) (flush-output)))
