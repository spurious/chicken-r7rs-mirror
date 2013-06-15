(module r7rs (define-library)

  (import (except scheme syntax-rules))		;XXX except ...
  (import chicken)			;XXX except ...
  (import numbers)
  (import scheme.base)
  (include "scheme.base-interface.scm")

  (begin-for-syntax
   (require-library r7rs-compile-time numbers))
  (import-for-syntax r7rs-compile-time matchable)

(require-library scheme.base)

(let ((old-hook ##sys#user-read-hook))
  (set! ##sys#user-read-hook
        (lambda (char port)
	  (define (fail tok)
	    (##sys#read-error port "invalid boolean literal syntax" tok))
          (case char
            ((#\f #\F #\t #\T)
	     (let ((sym (##sys#read port ##sys#default-read-info-hook)))
	       (if (not (symbol? sym))
		   (fail sym)
		   (let ((str (symbol->string sym)))
		     (cond ((or (string-ci=? "t" str) (string-ci=? "true" str)) #t)
			   ((or (string-ci=? "f" str) (string-ci=? "false" str)) #f)
			   (else (fail sym)))))))
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
