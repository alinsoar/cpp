;; -*- mode:scheme ; buffer-read-only:nil -*-

(define load-bnf-file
  (lambda (data)
    (make-hash
     (foldr (lambda (a b)
              (cons (cons (cadr a) (caddr a))
                    b))
            '()
            data))))

(define input-output-file
  (lambda (ret)
    (let ((args (current-command-line-arguments)))
      (or (and (= (vector-length args) 2)
               (file-exists? (vector-ref args 0)))
          (error "input-output-file"))
      (ret (file->list (vector-ref args 0))
           (lambda (PROC)
             (with-output-to-file (vector-ref args 1)
               PROC
               #:mode   'binary
               #:exists 'truncate/replace))))))


