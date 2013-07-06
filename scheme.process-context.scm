(module scheme.process-context (command-line
				exit
				emergency-exit
				;;XXX
				;get-environment-variable
				;get-environment-variables
				)

  (import scheme 
	  (rename chicken (exit chicken:exit))
	  foreign)

;;;
;;; 6.14. System interface.
;;;

(: command-line (-> (list-of string)))
(: exit (* -> noreturn))
(: emergency-exit (* -> noreturn))

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
     (chicken:exit 0))
    ((obj)
     (##sys#cleanup-before-exit)
     ;; ##sys#dynamic-unwind is hidden, have to unwind manually.
     ; (##sys#dynamic-unwind '() (length ##sys#dynamic-winds))
     (let unwind ()
       (unless (null? ##sys#dynamic-winds)
         (let ((after (cdar ##sys#dynamic-winds)))
           (set! ##sys#dynamic-winds (cdr ##sys#dynamic-winds))
           (after)
           (unwind))))
     (##core#inline "C_exit_runtime" (->exit-status obj)))))

(define emergency-exit
  (case-lambda
    (()
     (emergency-exit 0))
    ((obj)
     (##sys#cleanup-before-exit)
     ((foreign-lambda void "_exit" int) (->exit-status obj)))))

)
