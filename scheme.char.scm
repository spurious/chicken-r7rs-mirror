(module scheme.char (char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
		     string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)

(import
  (except scheme
	  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	  string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
  (prefix
    (only scheme
	  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	  string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
    %))

(import chicken)

;; Copy-pasta from scheme.base.scm.
(define-syntax define-extended-arity-comparator
  (syntax-rules ()
    ((_ name comparator check-type)
     (define name
       (let ((c comparator))
         (lambda (o1 o2 . os)
           (check-type o1 'name)
           (let lp ((o1 o1) (o2 o2) (os os) (eq #t))
             (check-type o2 'name)
             (if (null? os)
                 (and eq (c o1 o2))
                 (lp o2 (car os) (cdr os) (and eq (c o1 o2)))))))))))

(: char-ci=? (char char #!rest char -> boolean))
(: char-ci<? (char char #!rest char -> boolean))
(: char-ci>? (char char #!rest char -> boolean))
(: char-ci<=? (char char #!rest char -> boolean))
(: char-ci>=? (char char #!rest char -> boolean))

(define-extended-arity-comparator char-ci=? %char-ci=? ##sys#check-char)
(define-extended-arity-comparator char-ci<? %char-ci<? ##sys#check-char)
(define-extended-arity-comparator char-ci>? %char-ci>? ##sys#check-char)
(define-extended-arity-comparator char-ci<=? %char-ci<=? ##sys#check-char)
(define-extended-arity-comparator char-ci>=? %char-ci>=? ##sys#check-char)

(: string-ci=? (string string #!rest string -> boolean))
(: string-ci<? (string string #!rest string -> boolean))
(: string-ci>? (string string #!rest string -> boolean))
(: string-ci<=? (string string #!rest string -> boolean))
(: string-ci>=? (string string #!rest string -> boolean))

(define-extended-arity-comparator string-ci=? %string-ci=? ##sys#check-string)
(define-extended-arity-comparator string-ci<? %string-ci<? ##sys#check-string)
(define-extended-arity-comparator string-ci>? %string-ci>? ##sys#check-string)
(define-extended-arity-comparator string-ci<=? %string-ci<=? ##sys#check-string)
(define-extended-arity-comparator string-ci>=? %string-ci>=? ##sys#check-string))
