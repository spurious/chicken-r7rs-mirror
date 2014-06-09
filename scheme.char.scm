(module scheme.char (char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
		     string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
                     char-alphabetic? char-numeric? char-whitespace?
                     char-upper-case? char-lower-case?
		     char-foldcase string-foldcase
                     char-upcase char-downcase
		     string-upcase string-downcase
		     digit-value)

(import chicken)
(import r7rs-support)
(import
  (except scheme
	  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	  string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
  (prefix
    (only scheme
	  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	  string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
    %))

(require-library srfi-13)
(import (only srfi-13 string-map string-upcase string-downcase))

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
(define-extended-arity-comparator string-ci>=? %string-ci>=? ##sys#check-string)

(: char-foldcase (char -> char))
(define (char-foldcase c) (char-downcase c))

(: string-foldcase (string -> string))
(define (string-foldcase s) (string-map char-foldcase s))

(: digit-value (char -> (or fixnum false)))
(define (digit-value c)
  (let ((i (char->integer c)))
    (and (fx>= i 48) (fx<= i 57) (fx- i 48)))))
