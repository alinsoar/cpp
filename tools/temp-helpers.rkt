;; -*- mode:scheme ; buffer-read-only:nil -*-


(define stream->string
  (lambda (stream start-point end-point)
    (list->string
     (map
      integer->char
      (map
       (lambda (i)
         (vector-ref stream i))
       (range start-point end-point))))))


