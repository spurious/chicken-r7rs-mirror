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
(define-extended-arity-comparator string-ci>=? %string-ci>=? ##sys#check-string))
