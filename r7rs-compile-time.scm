;;;; compile-time support code (mostly for modules)


(import matchable)
(use srfi-1 files extras data-structures)


(define (parse-library-name name loc)
  (define (fail) (syntax-error loc "invalid library name" name))
  (match name
    ((? symbol?) name)
    ((parts ...)
     (string->symbol
      (string-intersperse 
       (map (lambda (part)
	      (cond ((symbol? part) (symbol->string part))
		    ((number? part) (number->string part))
		    (else (fail))))
	    parts)
       ".")))
    (_ (fail))))

(define (locate-library name loc)		; must be stripped
  ;;XXX scan include-path?
  (let* ((name2 (parse-library-name name loc))
	 (sname2 (symbol->string name2)))
    (or (file-exists? (string-append sname2 ".import.so"))
	(file-exists? (string-append sname2 ".import.scm"))
	(extension-information name2))))

(define (process-cond-expand clauses)
  ;; returns list of forms of successful clause or #f
  (define (fail msg . args)
    (apply
     syntax-error 
     msg
     (append args
	     `((cond-expand
		 ,@(map (lambda (clause) (cons (car clause) '(...))) clauses))))))
  (define (check test)
    (match test
      ('else #t)
      (('and tests ...) (every check tests))
      (('or tests ...) (any check tests))
      (('library name) (locate-library name 'cond-expand))
      ((? symbol? feature) (feature? feature))
      (_ (fail "invalid test expression in \"cond-expand\" form" test))))
  (let loop ((cs clauses))
    (match cs
      (() (fail "no clause applies in \"cond-expand\" form"))
      (((test body ...) . more)
       (if (check (strip-syntax test))
	   body
	   (loop more)))
      (else (fail "invalid \"cond-expand\" form")))))

(define (fixup-import/export-spec spec loc)
  (match spec
    (((and head (or 'only 'except 'rename 'prefix)) name . more)
     (cons* head (fixup-import/export-spec name loc) more))
    ((name ...)
     (parse-library-name name loc))
    (_ (syntax-error loc "invalid import/export specifier" spec))))

(define (current-source-filename)
  (or (and (feature? #:compiling) ##compiler#source-filename)
      ##sys#current-source-filename))

(define (read-forms filename ci?)
  (read-file 
   (if (absolute-pathname? filename)
       filename
       (make-pathname (current-source-filename) filename))
   (lambda (port)
     (parameterize ((case-sensitive ci?))
       (read port)))))

(define (parse-library-definition form dummy-export)	; expects stripped syntax
  (match form
    ((_ name decls ...)
     (let ((real-name (parse-library-name name 'define-library)))
       (define (parse-exports specs)
	 (map (match-lambda
		((and spec ('rename _ _))
		 (syntax-error
		  'define-library
		  "\"rename\" export specifier currently not supported" 
		  name))
		((? symbol? exp)
		 `(export ,exp))
		(spec (syntax-error 'define-library "invalid export specifier" spec name)))
	      specs))
       (define (parse-imports specs)
	 (map (lambda (spec)
		`(import ,(fixup-import/export-spec spec 'import)))
	      specs))
       (define (process-includes fnames ci?)
	 `(##core#begin
	   ,@(map (match-lambda
		    ((? string? fname)
		     `(##core#begin ,@(read-forms fname ci?)))
		    (fname (syntax-error 'include "invalid include-filename" fname)))
		  fnames)))
       (define (process-include-decls fnames)
	 (parse-decls (append-map (lambda (fname) (read-forms fname #t)) fnames)))
       (define (parse-decls decls)
	 (match decls
	   (() '(##core#begin))
	   ((('export specs ...) . more)
	    `(##core#begin
	      ,@(parse-exports specs)
	      ,(parse-decls more)))
	   ((('import specs ...) . more)
	    `(##core#begin
	      ,@(parse-imports specs)
	      ,(parse-decls more)))
	   ((('include fnames ...) . more)
	    `(##core#begin
	      ,@(process-includes fnames #f)
	      ,(parse-decls more)))
	   ((('include-ci fnames ...) . more)
	    `(##core#begin
	      ,@(process-includes fnames #t)
	      ,(parse-decls more)))
	   ((('include-library-declarations fnames ...) . more)
	    `(##core#begin
	      ,@(process-include-decls fnames)
	      ,(parse-decls more)))
	   ((('cond-expand decls ...) . more)
	    (parse-decls (process-cond-expand decls)))
	   ((('begin code ...) . more)
	    `(##core#begin 
	      (##core#begin ,@code) 
	      ,(parse-decls more)))
	   (decl (syntax-error 'define-library "invalid library declaration" decl))))
       `(##core#module ,real-name ((,dummy-export))
		       ;; gruesome hack: we add a dummy export for adding indirect exports
		       (import (rename scheme (define-syntax hidden:define-syntax)))
		       (import (only scheme.base import export)) ; overwrites existing "import"
		       (hidden:define-syntax ,dummy-export (lambda () #f))
		       ,(parse-decls decls))))
    (_ (syntax-error 'define-library "invalid library definition" form))))

(define (register-r7rs-module name)
  (let ((dummy (string->symbol (string-append (symbol->string name) "-dummy-export"))))
    (put! name '##r7rs#module dummy)
    dummy))

(set! ##sys#register-export
  (let ((register-export ##sys#register-export))
    (lambda (sym mod)
      (when mod
	(let-values (((explist ve se) (##sys#module-exports mod)))
	  (and-let* ((dummy (get (##sys#module-name mod) '##r7rs#module)))
	    (unless (eq? sym dummy)
	      (cond ((memq sym explist))
		    ((find (lambda (a) (and (pair? a) (eq? (car a) dummy))) explist) =>
		     (lambda (dummylist)
		       (set-cdr! dummylist (cons sym (cdr dummylist))))))))
	  (register-export sym mod))))))
