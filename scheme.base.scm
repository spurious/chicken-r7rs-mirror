(module scheme.base ()

(import (except scheme syntax-rules cond-expand
                       assoc list-set! list-tail member
                       char=? char<? char>? char<=? char>=?
                       string=? string<? string>? string<=? string>=?))
(import (prefix (only scheme char=? char<? char>? char<=? char>=?
                             string=? string<? string>? string<=? string>=?)
                %))
(import (except chicken with-exception-handler raise quotient remainder modulo))
(import numbers)

(include "scheme.base-interface.scm")

(begin-for-syntax (require-library r7rs-compile-time))
(import-for-syntax r7rs-compile-time)


(define-syntax import
  (er-macro-transformer
   (lambda (x r c)
     (##sys#expand-import 
      (cons (car x)
	    (map (lambda (spec)
		   (fixup-import/export-spec (strip-syntax spec) 'import))
		 (cdr x)))
      r c
      ##sys#current-environment ##sys#macro-environment
      #f #f 'import) ) ) )


;;;
;;; 4.2.1. Conditionals
;;;

(define-syntax cond-expand
  (er-macro-transformer
   (lambda (x r c)
     (cons (r 'begin)
	   (process-cond-expand (cdr x))))))


;;;
;;; 4.2.7. Exception handling
;;;

;; guard & guard-aux copied verbatim from the draft.
;; guard-aux put in a letrec-syntax due to import/export issues...
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     (letrec-syntax ((guard-aux 
                      (syntax-rules ___ (else =>)
                        ((guard-aux reraise (else result1 result2 ___))
                         (begin result1 result2 ___))
                        ((guard-aux reraise (test => result))
                         (let ((temp test))
                           (if temp
                               (result temp)
                               reraise)))
                        ((guard-aux reraise (test => result)
                                    clause1 clause2 ___)
                         (let ((temp test))
                           (if temp
                               (result temp)
                               (guard-aux reraise clause1 clause2 ___))))
                        ((guard-aux reraise (test))
                         (or test reraise))
                        ((guard-aux reraise (test) clause1 clause2 ___)
                         (let ((temp test))
                           (if temp
                               temp
                               (guard-aux reraise clause1 clause2 ___))))
                        ((guard-aux reraise (test result1 result2 ___))
                         (if test
                             (begin result1 result2 ___)
                             reraise))
                        ((guard-aux reraise
                                    (test result1 result2 ___)
                                    clause1 clause2 ___)
                         (if test
                             (begin result1 result2 ___)
                             (guard-aux reraise clause1 clause2 ___))))))
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
                    (apply values args))))))))))))))

;;;
;;; 5.4. Syntax definitions
;;;
(include "synrules.scm")


;;;
;;; 6.3 Booleans
;;;

;(: boolean=? ((procedure #:enforce) (boolean boolean #!rest boolean) boolean))
(: boolean=? (boolean boolean #!rest boolean -> boolean))

(define (boolean=? b1 b2 . rest)
  (##sys#check-boolean b1 'boolean=?)
  ;; Loop across all args, checking for booleans.  Don't shortcut and
  ;; stop when we find nonequality.
  (let lp ((b1 b1)
           (b2 b2)
           (rest rest)
           (result (eq? b1 b2)))
    (##sys#check-boolean b2 'boolean=?)
    (if (null? rest)
        (and result (eq? b1 b2))
        (lp b2 (car rest) (cdr rest) (and result (eq? b1 b2))))))


;;;
;;; 6.4 pairs and lists
;;;

(: make-list (forall (x) (fixnum #!optional x -> (list-of x))))

(define make-list
  (case-lambda
   ((n) (make-list n #f))
   ((n fill)
    (##sys#check-integer n 'make-list)
    (unless (fx>= n 0)
      (error 'make-list "not a positive integer" n))
    (do ((i n (fx- i 1))
         (result '() (cons fill result)))
        ((fx= i 0) result)))))


(: list-tail (forall (x) ((list-of x) fixnum -> (list-of x))))

(define (list-tail l n)
  (##sys#check-integer n 'list-tail)
  (unless (fx>= n 0)
    (error 'list-tail "not a positive integer" n))
  (do ((i n (fx- i 1))
       (result l (cdr result)))
      ((fx= i 0) result)
    (when (null? result)
      (error 'list-tail "out of range"))))


(: list-set! (list fixnum -> undefined))

(define (list-set! l n obj)
  (##sys#check-integer n 'list-set!)
  (unless (fx>= n 0)
    (error 'list-set! "not a positive integer" n))
  (do ((i n (fx- i 1))
       (l l (cdr l)))
      ((fx= i 0) (set-car! l obj))
    (when (null? l)
      (error 'list-set! "out of range"))))

(: member (forall (a b) (a (list-of b) #!optional (procedure (b a) *) ; sic
                         -> (or boolean (list-of b)))))

;; XXX These aren't exported to the types file!?
(define-specialization (member (x (or symbol procedure immediate)) (lst list))
  (##core#inline "C_u_i_memq" x lst))
(define-specialization (member x (lst (list-of (or symbol procedure immediate))))
  (##core#inline "C_u_i_memq" x lst))
(define-specialization (member x lst)
  (##core#inline "C_i_member" x lst))

(define member
  (case-lambda
   ((x lst) (##core#inline "C_i_member" x lst))
   ((x lst eq?)
    (let lp ((lst lst))
      (cond ((null? lst) #f)
            ((eq? (car lst) x) lst)
            (else (lp (cdr lst))))))))


(: assoc (forall (a b c) (a (list-of (pair b c)) #!optional (procedure (b a) *) ; sic
                            -> (or boolean (list-of (pair b c))))))

;; XXX These aren't exported to the types file!?
(define-specialization (assoc (x (or symbol procedure immediate)) (lst (list-of pair)))
  (##core#inline "C_u_i_assq" x lst))
(define-specialization (assoc x (lst (list-of (pair (or symbol procedure immediate) *))))
  (##core#inline "C_u_i_assq" x lst))
(define-specialization (assoc x lst)
  (##core#inline "C_i_assoc" x lst))

(define assoc
  (case-lambda
   ((x lst) (##core#inline "C_i_assoc" x lst))
   ((x lst eq?)
    (let lp ((lst lst))
      (cond ((null? lst) #f)
            ((not (pair? (car lst)))
             (error 'assoc "unexpected non-pair in list" (car lst)))
            ((eq? (caar lst) x) (car lst))
            (else (lp (cdr lst))))))))


(: list-copy (forall (a) ((list-of a) -> (list-of a))))

;; TODO: Test if this is the quickest way to do this, or whether we
;; should just cons recursively like our SRFI-1 implementation does.
(define (list-copy lst)
  (let lp ((res '())
           (lst lst))
    (if (null? lst)
        (##sys#fast-reverse res)
        (lp (cons (car lst) res) (cdr lst)))))

;;;
;;; 6.6 Characters
;;;

(define-syntax define-extended-arity-comparator
  (syntax-rules ()
    ((_ name comparator check-type)
     (define name
       (let ((cmp comparator))
         (lambda (o1 o2 . os)
           (check-type o1 'name)
           (let lp ((o1 o1) (o2 o2) (os os) (eq #t))
             (check-type o2 'name)
             (if (null? os)
                 (and eq (cmp o1 o2))
                 (lp o2 (car os) (cdr os) (and eq (cmp o1 o2)))))))))))

(: char=? (char char #!rest char -> boolean))
(: char<? (char char #!rest char -> boolean))
(: char>? (char char #!rest char -> boolean))
(: char<=? (char char #!rest char -> boolean))
(: char>=? (char char #!rest char -> boolean))

(define-extended-arity-comparator char=? %char=? ##sys#check-char)
(define-extended-arity-comparator char>? %char>? ##sys#check-char)
(define-extended-arity-comparator char<? %char<? ##sys#check-char)
(define-extended-arity-comparator char<=? %char<=? ##sys#check-char)
(define-extended-arity-comparator char>=? %char>=? ##sys#check-char)

;;;
;;; 6.7 Strings
;;;

(: string=? (string string #!rest string -> boolean))
(: string<? (string string #!rest string -> boolean))
(: string>? (string string #!rest string -> boolean))
(: string<=? (string string #!rest string -> boolean))
(: string>=? (string string #!rest string -> boolean))

(define-extended-arity-comparator string=? %string=? ##sys#check-string)
(define-extended-arity-comparator string<? %string<? ##sys#check-string)
(define-extended-arity-comparator string>? %string>? ##sys#check-string)
(define-extended-arity-comparator string<=? %string<=? ##sys#check-string)
(define-extended-arity-comparator string>=? %string>=? ##sys#check-string)

;;;
;;; 6.11. Exceptions
;;;

(: with-exception-handler ((* -> . *) (-> . *) -> . *))
(: raise (* -> noreturn))
(: raise-continuable (* -> . *))

;; XXX TODO: This is not threadsafe!
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
          ;; We might be interoperating with srfi-12 handlers set by intermediate
          ;; non-R7RS code, so check if a new handler was set in the meanwhile.
          (unless (eq? (car exception-handlers) ##sys#current-exception-handler)
            (set! exception-handlers
              (cons ##sys#current-exception-handler exception-handlers)))
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

(: error-object? (* --> boolean : (struct condition)))
(: error-object-message ((struct condition) -> string))
(: error-object-irritants ((struct condition) -> list))

(define error-object? condition?)
(define error-object-message (condition-property-accessor 'exn 'message))
(define error-object-irritants (condition-property-accessor 'exn 'arguments))

(: read-error? (* --> boolean))
(: file-error? (* --> boolean))

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

(: call-with-port (port (port -> . *) -> . *))
(: close-port (port -> void))
(: output-port-open? (output-port -> boolean))
(: input-port-open? (input-port -> boolean))
(: eof-object (--> eof))

(define (call-with-port port proc)
  (receive ret
      (proc port)
    (close-port port)
    (apply values ret)))

(define (close-port port)
  (cond ((input-port? port)
         (close-input-port port))
        ((output-port? port)
         (close-output-port port))
        (else
         (error 'close-port "not a port" port))))

(define (output-port-open? port)
  (##sys#check-output-port port #f 'output-port-open?)
  (not (port-closed? port)))
(define (input-port-open? port)
  (##sys#check-input-port port #f 'input-port-open?)
  (not (port-closed? port)))

(define (eof-object) #!eof)

)
