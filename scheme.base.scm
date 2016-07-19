(module scheme.base ()

(import (rename (except chicken vector-copy! with-exception-handler)
                (features feature-keywords)))

(import (except scheme syntax-rules assoc member list-tail
                       char=? char<? char>? char<=? char>=?
                       string=? string<? string>? string<=? string>=?
                       string-copy string->list vector->list vector-fill!))

(import (prefix (only scheme char=? char<? char>? char<=? char>=?
                             string=? string<? string>? string<=? string>=?)
                %))

(import (rename (only srfi-4 make-u8vector subu8vector u8vector u8vector?
                             u8vector-length u8vector-ref u8vector-set!
                             read-u8vector read-u8vector! write-u8vector)
                (u8vector bytevector)
                (u8vector-length bytevector-length)
                (u8vector-ref bytevector-u8-ref)
                (u8vector-set! bytevector-u8-set!)
                (u8vector? bytevector?)
                (make-u8vector make-bytevector)
                (write-u8vector write-bytevector)))

(include "scheme.base-interface.scm")

;; For syntax definition helpers.
(import-for-syntax r7rs-support)
(import-for-syntax r7rs-compile-time)
(begin-for-syntax
  (require-library r7rs-compile-time))
(import r7rs-support)

(cond-expand
  (no-numbers
   (import (only scheme modulo quotient remainder))
   (import (only (rename scheme (inexact->exact exact) (exact->inexact inexact)) exact inexact)))
  (else
   (import numbers)
   (export exact-integer? exact-integer-sqrt)
   (export floor/ floor-quotient floor-remainder)
   (export rationalize)
   (export truncate truncate/ truncate-quotient truncate-remainder)))

;; read/write-string/line/byte
(require-library extras)
(import (prefix (only extras read-string write-string) %))
(import (rename (only extras read-line read-byte write-byte)
                (read-byte read-u8)
                (write-byte write-u8)))

;; flush-output
(import (rename (only chicken flush-output)
                (flush-output flush-output-port)))

;; u8-ready?
(import (rename (only scheme char-ready?)
                (char-ready? u8-ready?)))

;; Non-R5RS string-*
(require-library srfi-13)
(import (prefix (only srfi-13 string-for-each string-map) %))
(import (only srfi-13 string-copy string-copy! string-fill! string->list))

;; For d-r-t redefinition.
(import-for-syntax (only chicken define-record-type))

;;;
;;; 4.1.7. Inclusion
;;;

(define-syntax include r7rs-include)
(define-syntax include-ci r7rs-include-ci)

;;;
;;; 4.2.1. Conditionals
;;;

(define-syntax cond-expand r7rs-cond-expand)

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
;;; 5.5 Record-type definitions
;;;

;; Rewrite the standard d-r-t expansion so that each newly-defined
;; record type has a unique type tag. This is every kind of hacky.
(define-syntax define-record-type
  (wrap-er-macro-transformer
   'define-record-type
   (lambda (e r c define-record-type)
     (let ((name (cadr e))
           (tag  (gensym "\x04r7rsrecord-type-tag")))
       `(##core#begin
         (##core#set! ,(r tag)
          (##sys#make-symbol ,(symbol->string name)))
         ,(let lp ((x (define-record-type e)))
            (cond ((equal? x `(##core#quote ,name)) (r tag))
                  ((pair? x) (cons (lp (car x)) (lp (cdr x))))
                  (else x))))))))

;;;
;;; 6.2.6 Numerical operations
;;;

(: square (number -> number))

(define (square n) (* n n))

;;;
;;; 6.3 Booleans
;;;

(: boolean=? (boolean boolean #!rest boolean -> boolean))

(define-extended-arity-comparator boolean=? eq? ##sys#check-boolean)


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


(: list-set! (list fixnum * -> undefined))

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
                         -> (or false (list-of b)))))

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
                            -> (or false (list-of (pair b c))))))

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


(: list-copy (forall (a) (a -> a)))

;; TODO: Test if this is the quickest way to do this, or whether we
;; should just cons recursively like our SRFI-1 implementation does.
(define (list-copy lst)
  (cond ((pair? lst)
         (let lp ((res '())
                  (lst lst))
           (if (pair? lst)
               (lp (cons (car lst) res) (cdr lst))
               (append (##sys#fast-reverse res) lst))))
        (else lst)))

;;;
;;; 6.5 Symbols
;;;

(: symbol=? (symbol symbol #!rest symbol -> boolean))

(define-extended-arity-comparator symbol=? eqv? ##sys#check-symbol)

;;;
;;; 6.6 Characters
;;;

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

(: string->vector (string #!optional fixnum fixnum -> (vector-of char)))
(: vector->string ((vector-of char) #!optional fixnum fixnum -> string))

(define string->vector
  (let ((s->v (lambda (s start . end)
                (##sys#check-string s 'string->vector)
                (let* ((len (##sys#size s))
                       (end (optional end len)))
                  (##sys#check-range start 0 (fx+ end 1) 'string->vector)
                  (##sys#check-range end start (fx+ len 1) 'string->vector)
                  (let ((v (##sys#make-vector (fx- end start))))
                    (do ((ti 0 (fx+ ti 1))
                         (fi start (fx+ fi 1)))
                        ((fx= fi end) v)
                      (##sys#setslot v ti (##core#inline "C_subchar" s fi))))))))
    (case-lambda
      ((s) (s->v s 0))
      ((s start) (s->v s start))
      ((s start end) (s->v s start end)))))

(define vector->string
  (let ((v->s (lambda (v start . end)
                (##sys#check-vector v 'vector->string)
                (let* ((len (##sys#size v))
                       (end (optional end len)))
                  (##sys#check-range start 0 (fx+ end 1) 'vector->string)
                  (##sys#check-range end start (fx+ len 1) 'vector->string)
                  (let ((s (##sys#make-string (fx- end start))))
                    (do ((ti 0 (fx+ ti 1))
                         (fi start (fx+ fi 1)))
                        ((fx= fi end) s)
                      (let ((c (##sys#slot v fi)))
                        (##sys#check-char c 'vector->string)
                        (##core#inline "C_setsubchar" s ti c))))))))
    (case-lambda
      ((v) (v->s v 0))
      ((v start) (v->s v start))
      ((v start end) (v->s v start end)))))

;;;
;;; 6.8. Vectors
;;;

(: vector-append (#!rest vector -> vector))
(: vector-copy (forall (a) ((vector-of a) #!optional fixnum fixnum -> (vector-of a))))
(: vector-copy! (vector fixnum vector #!optional fixnum fixnum -> undefined))
(: vector-fill! (vector * #!optional fixnum fixnum -> undefined))
(: vector->list (forall (a) ((vector-of a) #!optional fixnum fixnum -> (list-of a))))

(define vector-copy
  (let ((copy (lambda (v start . end)
                (##sys#check-vector v 'vector-copy)
                (let* ((len (##sys#size v))
                       (end (optional end len)))
                  (##sys#check-range start 0 (fx+ end 1) 'vector-copy)
                  (##sys#check-range end start (fx+ len 1) 'vector-copy)
                  (let ((vec (##sys#make-vector (fx- end start))))
                    (do ((ti 0 (fx+ ti 1))
                         (fi start (fx+ fi 1)))
                        ((fx>= fi end) vec)
                      (##sys#setslot vec ti (##sys#slot v fi))))))))
    (case-lambda
      ((v) (copy v 0))
      ((v start) (copy v start))
      ((v start end) (copy v start end)))))

(define vector-copy!
  (let ((copy! (lambda (to at from start . end)
                 (##sys#check-vector to 'vector-copy!)
                 (##sys#check-vector from 'vector-copy!)
                 (let* ((tlen (##sys#size to))
                        (flen (##sys#size from))
                        (end  (optional end flen)))
                   (##sys#check-range at 0 (fx+ tlen 1) 'vector-copy!)
                   (##sys#check-range start 0 (fx+ end 1) 'vector-copy!)
                   (##sys#check-range end start (fx+ flen 1) 'vector-copy!)
                   (##sys#check-range (fx- end start) 0 (fx+ (fx- tlen at) 1) 'vector-copy!)
                   (do ((fi start (fx+ fi 1))
                        (ti at (fx+ ti 1)))
                       ((fx= fi end))
                     (##sys#setslot to ti (##sys#slot from fi)))))))
    (case-lambda
      ((to at from) (copy! to at from 0))
      ((to at from start) (copy! to at from start))
      ((to at from start end) (copy! to at from start end)))))

(define vector-fill!
  (let ((fill! (lambda (v f start . end)
                 (##sys#check-vector v 'vector-fill!)
                 (let* ((len (##sys#size v))
                        (end (optional end len)))
                   (##sys#check-range start 0 (fx+ end 1) 'vector-fill!)
                   (##sys#check-range end start (fx+ len 1) 'vector-fill!)
                   (do ((i start (fx+ i 1)))
                       ((fx= i end))
                     (##sys#setslot v i f))))))
    (case-lambda
      ((v f) (fill! v f 0))
      ((v f start) (fill! v f start))
      ((v f start end) (fill! v f start end)))))

(define vector->list
  (let ((v->l (lambda (v start . end)
                (##sys#check-vector v 'vector->list)
                (let* ((len (##sys#size v))
                       (end (optional end len)))
                  (##sys#check-range start 0 (fx+ end 1) 'vector->list)
                  (##sys#check-range end start (fx+ len 1) 'vector->list)
                  (do ((i start (fx+ i 1))
                       (l '() (cons (##sys#slot v i) l)))
                      ((fx= i end) (##sys#fast-reverse l)))))))
    (case-lambda
      ((v) (v->l v 0))
      ((v start) (v->l v start))
      ((v start end) (v->l v start end)))))

(define (vector-append . vs)
  (##sys#for-each (cut ##sys#check-vector <> 'vector-append) vs)
  (let* ((lens (map ##sys#size vs))
         (vec  (##sys#make-vector (foldl fx+ 0 lens))))
    (do ((vs vs (cdr vs))
         (lens lens (cdr lens))
         (i 0 (fx+ i (car lens))))
        ((null? vs) vec)
      (vector-copy! vec i (car vs) 0 (car lens)))))

;;;
;;; 6.9. Bytevectors
;;;

(define-type bytevector u8vector)

(: bytevector (#!rest fixnum -> bytevector))
(: bytevector-append (#!rest bytevector -> bytevector))
(: bytevector-copy (bytevector #!optional fixnum fixnum -> bytevector))
(: bytevector-copy! (bytevector fixnum bytevector #!optional fixnum fixnum -> undefined))
(: bytevector-length (bytevector -> fixnum))
(: bytevector-u8-ref (bytevector fixnum -> fixnum))
(: bytevector-u8-set! (bytevector fixnum fixnum -> void))
(: bytevector? (* -> boolean : bytevector))
(: make-bytevector (fixnum #!optional fixnum -> bytevector))
(: string->utf8 (string #!optional fixnum fixnum -> bytevector))
(: utf8->string (bytevector #!optional fixnum fixnum -> string))
(: write-bytevector (bytevector #!optional output-port -> fixnum))

(define bytevector-copy
  (case-lambda
    ((bv)
     (##sys#check-structure bv 'u8vector 'bytevector-copy)
     (subu8vector bv 0 (bytevector-length bv)))
    ((bv start)
     (##sys#check-structure bv 'u8vector 'bytevector-copy)
     (subu8vector bv start (bytevector-length bv)))
    ((bv start end)
     (subu8vector bv start end))))

(define bytevector-copy!
  (let ((copy! (lambda (to at from start . end)
                 (##sys#check-structure to 'u8vector 'bytevector-copy!)
                 (##sys#check-structure from 'u8vector 'bytevector-copy!)
                 (let* ((tlen (bytevector-length to))
                        (flen (bytevector-length from))
                        (end  (optional end flen)))
                   (##sys#check-range at 0 (fx+ tlen 1) 'bytevector-copy!)
                   (##sys#check-range start 0 (fx+ end 1) 'bytevector-copy!)
                   (##sys#check-range end start (fx+ flen 1) 'bytevector-copy!)
                   (##sys#check-range (fx- end start) 0 (fx+ (fx- tlen at) 1) 'bytevector-copy!)
                   (do ((fi start (fx+ fi 1))
                        (ti at (fx+ ti 1)))
                       ((fx= fi end))
                     (bytevector-u8-set! to ti (bytevector-u8-ref from fi)))))))
    (case-lambda
      ((to at from) (copy! to at from 0))
      ((to at from start) (copy! to at from start))
      ((to at from start end) (copy! to at from start end)))))

(define (bytevector-append . bvs)
  (##sys#for-each (cut ##sys#check-structure <> 'u8vector 'bytevector-append) bvs)
  (let* ((lens (map bytevector-length bvs))
         (bv   (make-bytevector (foldl fx+ 0 lens))))
    (do ((bvs bvs (cdr bvs))
         (lens lens (cdr lens))
         (i 0 (fx+ i (car lens))))
        ((null? bvs) bv)
      (bytevector-copy! bv i (car bvs) 0 (car lens)))))

(define utf8->string
  (let ((bv->s (lambda (bv start . end)
                (##sys#check-structure bv 'u8vector 'utf8->string)
                (let* ((len (bytevector-length bv))
                       (end (optional end len)))
                  (##sys#check-range start 0 (fx+ end 1) 'utf8->string)
                  (##sys#check-range end start (fx+ len 1) 'utf8->string)
                  (let ((s (##sys#make-string (fx- end start))))
                    (do ((si 0 (fx+ si 1))
                         (vi start (fx+ vi 1)))
                        ((fx= vi end) s)
                      (##sys#setbyte s si (bytevector-u8-ref bv vi))))))))
    (case-lambda
      ((bv) (bv->s bv 0))
      ((bv start) (bv->s bv start))
      ((bv start end) (bv->s bv start end)))))

(define string->utf8
  (let ((s->bv (lambda (s start . end)
                (##sys#check-string s 'string->utf8)
                (let* ((len (##sys#size s))
                       (end (optional end len)))
                  (##sys#check-range start 0 (fx+ end 1) 'string->utf8)
                  (##sys#check-range end start (fx+ len 1) 'string->utf8)
                  (let ((bv (make-bytevector (fx- end start))))
                    (do ((vi 0 (fx+ vi 1))
                         (si start (fx+ si 1)))
                        ((fx= si end) bv)
                      (bytevector-u8-set! bv vi (##sys#byte s si))))))))
    (case-lambda
      ((s) (s->bv s 0))
      ((s start) (s->bv s start))
      ((s start end) (s->bv s start end)))))

;;;
;;; 6.10. Control features
;;;

(: string-for-each ((char #!rest char -> *) string #!rest string -> void))
(: string-map ((char #!rest char -> char) string #!rest string -> string))
(: vector-for-each ((* #!rest * -> *) vector #!rest vector -> void))
(: vector-map ((* #!rest * -> *) vector #!rest vector -> vector))

(define string-map
  (case-lambda
    ((proc str)
     (%string-map proc str))
    ((proc . strs)
     (##sys#check-closure proc 'string-map)
     (##sys#for-each (cut ##sys#check-string <> 'string-map) strs)
     (let* ((len (foldl fxmin most-positive-fixnum (map ##sys#size strs)))
            (str (##sys#make-string len)))
       (do ((i 0 (fx+ i 1)))
           ((fx= i len) str)
         (string-set! str i (apply proc (map (cut string-ref <> i) strs))))))))

(define string-for-each
  (case-lambda
    ((proc str)
     (%string-for-each proc str))
    ((proc . strs)
     (##sys#check-closure proc 'string-for-each)
     (##sys#for-each (cut ##sys#check-string <> 'string-for-each) strs)
     (let* ((len (foldl fxmin most-positive-fixnum (map ##sys#size strs)))
            (str (##sys#make-string len)))
       (do ((i 0 (fx+ i 1)))
           ((fx= i len) str)
         (apply proc (map (cut string-ref <> i) strs)))))))

(define vector-map
  (case-lambda
    ((proc v)
     (##sys#check-closure proc 'vector-map)
     (##sys#check-vector v 'vector-map)
     (let* ((len (##sys#size v))
            (vec (##sys#make-vector len)))
       (do ((i 0 (fx+ i 1)))
           ((fx= i len) vec)
        (##sys#setslot vec i (proc (##sys#slot v i))))))
    ((proc . vs)
     (##sys#check-closure proc 'vector-map)
     (##sys#for-each (cut ##sys#check-vector <> 'vector-map) vs)
     (let* ((len (foldl fxmin most-positive-fixnum (map ##sys#size vs)))
            (vec (##sys#make-vector len)))
       (do ((i 0 (fx+ i 1)))
           ((fx= i len) vec)
         (##sys#setslot vec i (apply proc (map (cut vector-ref <> i) vs))))))))

(define vector-for-each
  (case-lambda
    ((proc v)
     (##sys#check-closure proc 'vector-for-each)
     (##sys#check-vector v 'vector-for-each)
     (let ((len (##sys#size v)))
       (do ((i 0 (fx+ i 1)))
           ((fx= i len))
         (proc (##sys#slot v i)))))
    ((proc . vs)
     (##sys#check-closure proc 'vector-for-each)
     (##sys#for-each (cut ##sys#check-vector <> 'vector-for-each) vs)
     (let* ((len (foldl fxmin most-positive-fixnum (map ##sys#size vs)))
            (vec (##sys#make-vector len)))
       (do ((i 0 (fx+ i 1)))
           ((fx= i len) vec)
         (apply proc (map (cut vector-ref <> i) vs)))))))

;;;
;;; 6.11. Exceptions
;;;

(: with-exception-handler ((* -> . *) (-> . *) -> . *))
(: raise (* -> noreturn))
(: raise-continuable (* -> . *))

(define with-exception-handler)
(define raise)
(define raise-continuable)

;; XXX TODO: This is not threadsafe!
(let ((exception-handlers
       (let ((lst (list ##sys#current-exception-handler)))
         (set-cdr! lst lst)
         lst)))
  (set! with-exception-handler
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
         (set! ##sys#current-exception-handler (car exception-handlers))))))
   (set! raise
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
            'location #f))))))
   (set! raise-continuable
     (lambda (obj)
       (with-exception-handler
        (cadr exception-handlers)
        (lambda ()
          ((cadr exception-handlers) obj))))))

(: error-object? (* -> boolean : (struct condition)))
(: error-object-message ((struct condition) -> (or string false)))
(: error-object-irritants ((struct condition) -> (or list false)))

(define (error-object? o) (condition? o))
(define error-object-message (condition-property-accessor 'exn 'message))
(define error-object-irritants (condition-property-accessor 'exn 'arguments))

(: read-error? (* --> boolean))
(: file-error? (* --> boolean))

(define read-error?)
(define file-error?)

(let ((exn?    (condition-predicate 'exn))
      (i/o?    (condition-predicate 'i/o))
      (file?   (condition-predicate 'file))
      (syntax? (condition-predicate 'syntax)))
  (set! read-error?
    (lambda (obj)
      (and (exn? obj)
           (or (i/o? obj) ; XXX Not fine-grained enough.
               (syntax? obj)))))
  (set! file-error?
    (lambda (obj)
      (and (exn? obj)
           (file? obj)))))

;;;
;;; 6.13. Input and Output
;;;

(: binary-port? (* --> boolean : port?))
(: call-with-port (port (port -> . *) -> . *))
(: close-port (port -> void))
(: eof-object (--> eof))
(: input-port-open? (input-port -> boolean))
(: output-port-open? (output-port -> boolean))
(: peek-u8 (#!optional input-port -> fixnum))
(: read-bytevector (number #!optional input-port -> (or bytevector eof)))
(: read-bytevector! (bytevector #!optional input-port number number -> fixnum))
(: read-string (number #!optional input-port -> (or string eof)))
(: read-u8 (#!optional input-port -> fixnum))
(: textual-port? (* --> boolean : port?))
(: u8-ready? (#!optional input-port -> boolean))
(: write-string (string #!optional output-port fixnum fixnum -> void))
(: write-u8 (fixnum #!optional output-port -> void))

;; CHICKEN's ports can handle both.
(define (binary-port? port) (port? port))
(define (textual-port? port) (port? port))

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

(define peek-u8
  (case-lambda
    (()
     (##sys#check-input-port ##sys#standard-input #t 'peek-u8)
     (let ((c (peek-char ##sys#standard-input)))
       (if (eof-object? c) c
           (char->integer c))))
    ((port)
     (##sys#check-input-port port #t 'peek-u8)
     (let ((c (peek-char port)))
       (if (eof-object? c) c
           (char->integer c))))))

(define read-string
  (let ((read-string/eof
         (lambda (k port)
           (##sys#check-input-port port #t 'read-string)
           (if (eof-object? (peek-char port))
               #!eof
               (##sys#read-string/port k port)))))
    (case-lambda
      ((k)
       (read-string/eof k ##sys#standard-input))
      ((k port)
       (read-string/eof k port)))))

(define write-string
  (case-lambda
    ((s)
     (%write-string s #f ##sys#standard-output))
    ((s port)
     (%write-string s #f port))
    ((s port start)
     (##sys#check-string s 'write-string)
     (let ((len (##sys#size s)))
       (##sys#check-range start 0 (fx+ len 1) 'write-string)
       (%write-string (##sys#substring s start len) #f port)))
    ((s port start end)
     (##sys#check-string s 'write-string)
     (##sys#check-range start 0 (fx+ end 1) 'write-string)
     (##sys#check-range end start (fx+ (##sys#size s) 1) 'write-string)
     (%write-string (##sys#substring s start end) #f port))))

(define read-bytevector
  (let ((read-u8vector/eof
         (lambda (k port)
           (let ((bv (read-u8vector k port)))
             (if (fx= 0 (bytevector-length bv)) #!eof bv)))))
    (case-lambda
      ((k)
       (read-u8vector/eof k ##sys#standard-input))
      ((k port)
       (read-u8vector/eof k port)))))

(define read-bytevector!
  (let ((read-u8vector!/eof
         (lambda (k bv port . args)
           (let ((r (apply read-u8vector! k bv port args)))
             (if (fx= r 0) #!eof r)))))
    (case-lambda
      ((bv)
       (read-u8vector!/eof #f bv ##sys#standard-input))
      ((bv port)
       (read-u8vector!/eof #f bv port))
      ((bv port start)
       (read-u8vector!/eof #f bv port start))
      ((bv port start end)
       (read-u8vector!/eof (fx- end start) bv port start)))))

(define (open-input-bytevector bv)
  (let ((port (##sys#make-port #t #f "(bytevector)" 'custom)))
    (##sys#setslot
     port
     2
     (let ((index 0)
           (bv-len (bytevector-length bv)))
       (vector (lambda (_) ; read-char
                 (if (fx= index bv-len)
                     (eof-object)
                     (let ((c (bytevector-u8-ref bv index)))
                       (set! index (fx+ index 1))
                       (integer->char c))))
               (lambda (_) ; peek-char
                 (if (fx= index bv-len)
                     (eof-object)
                     (bytevector-u8-ref bv index)))
               #f    ; write-char
               #f    ; write-string
               (lambda (_) ; close
                 (##sys#setislot port 8 #t))
               #f    ; flush-output
               (lambda (_) ; char-ready?
                 (not (fx= index bv-len)))
               #f    ; read-string!
               #f    ; read-line
               #f))) ; read-buffered
     port))

(define (open-output-bytevector) (open-output-string))

(define (get-output-bytevector p)
  (string->utf8 (get-output-string p)))

;;;
;;; 6.14. System interface
;;;

(: features (--> (list-of symbol)))

(define (features)
  (map (lambda (s)
         (##sys#string->symbol (##sys#symbol->string s)))
       (feature-keywords))))
