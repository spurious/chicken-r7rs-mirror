(module r7rs

(
 ;; Exceptions
 raise
 raise-continuable
 error-object?
 error-object-message
 error-object-irritants
 read-error?
 file-error?
 ; TODO guard
 )

(import chicken scheme)
(use srfi-13)

(define (read-asserted-ci-symbol port valid-symbols error-message)
  (let ((sym (##sys#read port ##sys#default-read-info-hook)))
    (or (and (symbol? sym)
             (memq (string->symbol (string-downcase (symbol->string sym))) valid-symbols))
        (##sys#read-error port error-message sym))))

(let ((old-hook ##sys#user-read-hook))
  (set! ##sys#user-read-hook
        (lambda (char port)
          (case char
            ((#\f #\F)
             (read-asserted-ci-symbol port '(f false) "invalid `false' read syntax")
             #f)
            ((#\t #\T)
             (read-asserted-ci-symbol port '(t true) "invalid `true' read syntax")
             #t)
            (else (old-hook char port))))))

;;;
;;; 6.11. Exceptions
;;;

(define raise abort)
(define raise-continuable signal)
(define error-object? condition?)
(define error-object-message (condition-property-accessor 'exn 'message))
(define error-object-irritants (condition-property-accessor 'exn 'arguments))

(define-values (read-error? file-error?)
  (let ((exn?    (condition-predicate 'exn))
        (i/o?    (condition-predicate 'i/o))
        (file?   (condition-predicate 'file))
        (syntax? (condition-predicate 'syntax)))
    (values
     ;; read-error?
     (lambda (obj)
       (and (exn? obj)
            (or (i/o? obj) ; XXX Not fine-grained enough.
                (syntax? obj))))
     ;; file-error?
     (lambda (obj)
       (and (exn? obj)
            (file? obj))))))

)
