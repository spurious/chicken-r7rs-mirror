(module r7rs

(
 ;; Exceptions
 raise
 raise-continuable
 error-object?
 error-object-message
 error-object-irritants
 read-error?
 file-error?
 ; TODO guard
 ;; System interface
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
;;; 6.11. Exceptions
;;;

(define raise abort)
(define raise-continuable signal)
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
;;; 6.14. System interface.
;;;

;; Should these go in a separate module (process-context)?

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
