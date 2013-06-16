(module scheme.eval (eval
		     environment)

  (import scheme chicken)
  (use r7rs-compile-time)

;;;
;;; 6.12. Environments and evaluation
;;;

  (define (environment . specs)
    (let ((name (gensym "environment-module-")))
      ;; create module...
      (eval `(module ,name ()
	       ,@(map (lambda (spec)
			`(import ,(fixup-import/export-spec spec 'environment)))
		      specs)))
      (let ((env (module-environment name)))
	;; ...and remove it right away
	(set! ##sys#module-table (##sys#delq (assq name ##sys#module-table) ##sys#module-table))
	env)))

)
