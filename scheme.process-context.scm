(module scheme.process-context (command-line
				emergency-exit
				exit
				get-environment-variable
				get-environment-variables)
  (import scheme
          chicken.process-context
          chicken.type
	  (rename chicken.base (exit chicken-exit)))

;;;
;;; 6.14. System interface.
;;;

(: command-line (-> (list-of string)))
(: exit (#!optional * -> noreturn))
(: emergency-exit (#!optional * -> noreturn))

(define (command-line)
  ;; Don't cache these; they may be parameterized at any time!
  (cons (program-name) (command-line-arguments)))

(define (->exit-status obj)
  (cond ((integer? obj) obj)
        ((eq? obj #f) 1)
        (else 0)))

(define exit
  (case-lambda
    (()
     (exit 0))
    ((obj)
     ;; ##sys#dynamic-unwind is hidden, have to unwind manually.
     ; (##sys#dynamic-unwind '() (length ##sys#dynamic-winds))
     (let unwind ()
       (unless (null? ##sys#dynamic-winds)
         (let ((after (cdar ##sys#dynamic-winds)))
           (set! ##sys#dynamic-winds (cdr ##sys#dynamic-winds))
           (after)
           (unwind))))
     ;; The built-in exit runs cleanup handlers for us.
     (chicken-exit (->exit-status obj)))))

)
