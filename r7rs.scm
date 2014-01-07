(module r7rs (define-library import import-for-syntax export syntax-rules)

  (import (except scheme syntax-rules))	;XXX except ...
  (import (only chicken include))	;XXX except ...

  ;; For syntax definition helpers.
  (import-for-syntax r7rs-compile-time matchable)
  (begin-for-syntax (require-library r7rs-compile-time))

  ;; For extended number literals.
  (require-library numbers)

  ;; For #u8(...) syntax.
  (require-extension srfi-4)

(let ((old-hook ##sys#user-read-hook))
  ;; XXX Read syntax for "#false" and srfi-4's "#f32(...)" and friends
  ;; don't play nicely together, so we have to copy some of srfi-4.scm's
  ;; read hook here, to fall back on when we hit a vector of floats.
  (define read-srfi-4-vector
    (let ([consers (list 'u8 list->u8vector
			 's8 list->s8vector
			 'u16 list->u16vector
			 's16 list->s16vector
			 'u32 list->u32vector
			 's32 list->s32vector
			 'f32 list->f32vector
			 'f64 list->f64vector)])
      (lambda (tag port)
	(let* ([x (read port)])
	  (cond [(or (eq? tag 'f) (eq? tag 'F)) #f]
		[(memq tag consers) => (lambda (c) ((##sys#slot (##sys#slot c 1) 0) (read port)))]
		[else (##sys#read-error port "illegal bytevector syntax" tag)])))))
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
			   (else (read-srfi-4-vector sym port)))))))
            (else (old-hook char port))))))

;;;
;;; 5.2. Import declarations
;;;

(define-syntax import (import-transformer 'import))
(define-syntax import-for-syntax (import-transformer 'import-for-syntax))

;;;
;;; 5.4. Syntax definitions
;;;
(include "synrules.scm")

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
       (_ (syntax-error 'define-library "invalid library definition" x)))))))
