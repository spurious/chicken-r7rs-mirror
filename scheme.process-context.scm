(module scheme.process-context (command-line
				emergency-exit
				exit
				get-environment-variable
				get-environment-variables)

  (import scheme 
	  (rename chicken (exit chicken-exit))
	  foreign)

;;;
;;; 6.14. System interface.
;;;

(: command-line (-> (list-of string)))
(: get-environment-variables (-> (list-of (pair string string))))
(: exit (#!optional * -> noreturn))
(: emergency-exit (#!optional * -> noreturn))

(define (command-line)
  ;; Don't cache these; they may be parameterized at any time!
  (cons (program-name) (command-line-arguments)))

;; XXX get-environment-variables copied from posixunix.scm.
;; (And not actually expected to work on other platforms yet.)

#>
#ifdef __APPLE__
# include <crt_externs.h>
# define C_getenventry(i)       ((*_NSGetEnviron())[ i ])
#else
extern char **environ;
# define C_getenventry(i)       (environ[ i ])
#endif
<#

(define get-environment-variables
  (let ([get (foreign-lambda c-string "C_getenventry" int)])
    (lambda ()
      (let loop ([i 0])
        (let ([entry (get i)])
          (if entry
              (let scan ([j 0])
                (if (char=? #\= (##core#inline "C_subchar" entry j))
                    (cons (cons (##sys#substring entry 0 j)
                                (##sys#substring entry (fx+ j 1) (##sys#size entry)))
                          (loop (fx+ i 1)))
                    (scan (fx+ j 1)) ) )
              '()))))))

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

(define emergency-exit
  (case-lambda
    (()
     (emergency-exit 0))
    ((obj)
     (##sys#cleanup-before-exit)
     ((foreign-lambda void "_exit" int) (->exit-status obj)))))

)
