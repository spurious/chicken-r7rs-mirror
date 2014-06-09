;;;
;;; Support code for building the R7RS extension.
;;;

(module r7rs-support *

  (import scheme chicken)

  (define (macro-handler name)
    (cond ((assq name (##sys#macro-environment)) => caddr)
          (else #f)))

  (define (wrap-er-macro-transformer name handler)
    (er-macro-transformer
     (let ((orig (macro-handler name)))
       (lambda (x r c)
         (let ((e (##sys#current-environment)))
           (handler x r c (lambda (x*) (orig x* '() e))))))))

  (define-syntax define-extended-arity-comparator
    (syntax-rules ()
      ((_ name comparator check-type)
       (define name
         (let ((c comparator))
           (lambda (o1 o2 . os)
             (check-type o1 'name)
             (let lp ((o1 o1) (o2 o2) (os os) (eq #t))
               (check-type o2 'name)
               (if (null? os)
                   (and eq (c o1 o2))
                   (lp o2 (car os) (cdr os) (and eq (c o1 o2))))))))))))
