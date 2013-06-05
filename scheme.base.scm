(module scheme.base ()

(import (except scheme cond-expand))
(import (except chicken with-exception-handler raise))

(include "scheme.base-interface.scm")

(begin-for-syntax (require-library r7rs-compile-time))
(import-for-syntax r7rs-compile-time)


;;;
;;; 4.2.1. Conditionals
;;;

(define-syntax cond-expand
  (er-macro-transformer
   (lambda (x r c)
     (process-cond-expand (cdr x)))))


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


)
