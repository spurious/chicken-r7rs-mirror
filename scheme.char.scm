(module scheme.char (char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
		     string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
                     char-alphabetic? char-numeric? char-whitespace?
                     char-upper-case? char-lower-case?
                     ;; char-foldcase string-foldcase ; TODO
                     char-upcase char-downcase
		     string-upcase string-downcase
		     digit-value)

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
(import (only srfi-13 string-upcase string-downcase))

(import chicken)
(require-extension r7rs-compile-time)

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

(: digit-value (char -> (or fixnum boolean)))

(define (digit-value c)
  (let ((i (char->integer c)))
    (and (fx>= i 48) (fx<= i 57) (fx- i 48)))))
