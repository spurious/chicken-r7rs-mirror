(module scheme.eval (eval
		     environment)

  (import (rename scheme (eval %eval)) chicken)
  (import r7rs-compile-time)

;;;
;;; 6.12. Environments and evaluation
;;;

  (: eval (* (struct environment) -> *))

  (define (eval expr env) (%eval expr env))

  (: environment (list -> (struct environment)))

  (define (environment . specs)
    (let ((name (gensym "environment-module-")))
      ;; create module...
      (%eval `(module ,name ()
		,@(map (lambda (spec)
			 `(import ,(fixup-import/export-spec spec 'environment)))
		       specs)))
      (let ((mod (##sys#find-module name)))
	;; ...and remove it right away
	(set! ##sys#module-table (##sys#delq mod ##sys#module-table))
	(##sys#make-structure 'environment
	 name
	 (let ((env (##sys#slot mod 13)))
	   (append (car env) (cdr env))) ; combine env and syntax bindings
	 #t))))

)
