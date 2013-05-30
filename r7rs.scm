(module r7rs

(
 ;; Exception handling
 guard
 ;; Exceptions
 with-exception-handler
 raise
 raise-continuable
 error-object?
 error-object-message
 error-object-irritants
 read-error?
 file-error?
 ;; Input & output
 call-with-port
 close-port
 eof-object
 ;; System interface
 command-line
 exit
 emergency-exit
 )

(import chicken scheme foreign)
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
;;; 4.2.7. Exception handling
;;;

;; guard & guard-aux copied verbatim from the draft.
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
        (lambda (guard-k)
          (with-exception-handler
           (lambda (condition)
             ((call/cc
                (lambda (handler-k)
                  (guard-k
                   (lambda ()
                     (let ((var condition))
                       (guard-aux
                        (handler-k
                         (lambda ()
                           (raise-continuable condition)))
                        clause ...))))))))
           (lambda ()
             (call-with-values
              (lambda () e1 e2 ...)
              (lambda args
                (guard-k
                 (lambda ()
                   (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

;;;
;;; 6.11. Exceptions
;;;

(define-values (with-exception-handler raise raise-continuable)
  (let ((exception-handlers
         (let ((lst (list ##sys#current-exception-handler)))
           (set-cdr! lst lst)
           lst)))
    (values
     ;; with-exception-handler
     (lambda (handler thunk)
       (dynamic-wind
        (lambda ()
          (set! exception-handlers (cons handler exception-handlers))
          (set! ##sys#current-exception-handler handler))
        thunk
        (lambda ()
          (set! exception-handlers (cdr exception-handlers))
          (set! ##sys#current-exception-handler (car exception-handlers)))))
     ;; raise
     (lambda (obj)
       (with-exception-handler
        (cadr exception-handlers)
        (lambda ()
          ((cadr exception-handlers) obj)
          ((car exception-handlers)
           (make-property-condition
            'exn
            'message "exception handler returned"
            'arguments '()
            'location #f)))))
     ;; raise-continuable
     (lambda (obj)
       (with-exception-handler
        (cadr exception-handlers)
        (lambda ()
          ((cadr exception-handlers) obj)))))))

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

;;;
;;; 6.13. Input and Output
;;;

(define (call-with-port port proc)
  (dynamic-wind void (lambda () (proc port)) (lambda () (close-port port))))

(define (close-port port)
  (cond ((input-port? port)
         (close-input-port port))
        ((output-port? port)
         (close-output-port port))
        (else
         (error 'close-port "not a port" port))))

(define (eof-object) #!eof)

;;;
;;; 6.14. System interface.
;;;

;; Should these go in a separate module (process-context)?

(define command-line
  (let ((command-line #f)
        (arguments (command-line-arguments)))
    (lambda ()
      (unless command-line
        (set! command-line (cons (program-name) arguments)))
      command-line)))

(define (->exit-status obj)
  (cond ((integer? obj) obj)
        ((eq? obj #f) 1)
        (else 0)))

(define exit
  (case-lambda
    (()
     (exit 0))
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
