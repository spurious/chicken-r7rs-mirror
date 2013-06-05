(module r7rs (define-library)

  (import scheme)			;XXX except ...
  (import chicken)			;XXX except ...
  (import numbers)
  (import scheme.base)
  (include "scheme.base-interface.scm") 

  (begin-for-syntax
   (require-library r7rs-compile-time numbers))
  (import-for-syntax r7rs-compile-time matchable)

(use srfi-13)				;XXX get rid of this! (used for "string-downcase"?)

(require-library scheme.base)

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
;;; 5.6.1. Libraries
;;;

(define-syntax define-library
  (er-macro-transformer
   (lambda (x r c)
     (match (strip-syntax x)
       ((_ name decls ...)
	(let ((dummy (register-r7rs-module (parse-library-name name 'define-library))))
	  (parse-library-definition x dummy)))
       (_ (syntax-error 'define-library "invalid library definition" x))))))


)
