;;;; compile-time support code (mostly for modules)


(import matchable)
(use srfi-1 files extras data-structures)

(define (parse-library-name name loc)
  (define (fail) (syntax-error loc "invalid library name" name))
  (match name
    ((? symbol?) name)
    ;; We must replicate the core magic that handles SRFI-55's
    ;; (require-extension (srfi N)), because we also need to generate
    ;; SRFI-N library names when defining SRFIs from an R7RS module.
    (('srfi (and num (? fixnum?)))
     (string->symbol (string-append "srfi-" (number->string num))))
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
    (or (##sys#provided? name2)
	(file-exists? (string-append sname2 ".import.so"))
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
      (('not test) (not (check test)))
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

(define (fixup-import/export-spec spec loc) ; expects spec to be stripped
  (match spec
    (((and head (or 'only 'except 'rename 'prefix)) name . more)
     (cons* head (fixup-import/export-spec name loc) more))
    ((name ...)
     (parse-library-name name loc))
    ((? symbol? spec) spec)
    (_ (syntax-error loc "invalid import/export specifier" spec))))

;; Dig e.g. foo.bar out of (only (foo bar) ...) ...
(define (import/export-spec-feature-name spec loc)
  (match spec
    ((? symbol? spec) spec)
    (((or 'only 'except 'rename 'prefix) name . more)
     (import/export-spec-feature-name name loc))
    ((name ...)
     (parse-library-name name loc))
    (else
     (syntax-error loc "invalid import/export specifier" spec))))

(define (wrap-er-macro-transformer name handler)
  (er-macro-transformer
   (let ((orig (caddr (assq name (##sys#macro-environment)))))
     (lambda (x r c)
       (let ((e (##sys#current-environment)))
         (handler x r c (lambda (x*) (orig x* '() e))))))))

(define (import-transformer type)
  (wrap-er-macro-transformer
   type
   (lambda (x r c import)
     `(##core#begin
       ,@(map (lambda (spec)
                (let ((spec (fixup-import/export-spec spec type))
                      (name (import/export-spec-feature-name spec type)))
                  (import (list type spec))
                  (if (memq name '(scheme foreign)) ; XXX others?
                      '(##core#undefined)
                      `(##core#require-extension (,name) #f))))
              (strip-syntax (cdr x)))))))

(define (read-forms filename ci?)
  (parameterize ((case-sensitive (not ci?)))
    (##sys#include-forms-from-file filename)))

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
	 ;; What R7RS calls IMPORT, we call USE (it imports *and* loads code)
	 ;; XXX TODO: Should be import-for-syntax'ed as well?
	 `(import ,@specs)) ; NOTE this is the r7rs module's IMPORT!
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
	      ,(parse-imports specs)
	      ,(parse-decls more)))
	   ((('include fnames ...) . more)
	    `(##core#begin
	      ,(process-includes fnames #f)
	      ,(parse-decls more)))
	   ((('include-ci fnames ...) . more)
	    `(##core#begin
	      ,(process-includes fnames #t)
	      ,(parse-decls more)))
	   ((('include-library-declarations fnames ...) . more)
	    `(##core#begin
	      ,@(process-include-decls fnames)
	      ,(parse-decls more)))
	   ((('cond-expand decls ...) . more)
	    `(##core#begin
	      ,@(process-cond-expand decls)
	      ,(parse-decls more)))
	   ((('begin code ...) . more)
	    `(##core#begin 
	      ,@code
	      ,(parse-decls more)))
	   (decl (syntax-error 'define-library "invalid library declaration" decl))))
       `(##core#begin
	 (##core#module
	  ,real-name ((,dummy-export))
	  ;; gruesome hack: we add a dummy export for adding indirect exports
	  (##core#define-syntax ,dummy-export (##core#lambda _ '(##core#undefined)))
	  ;; Another gruesome hack: provide feature so "use" works properly
	  (##sys#provide (##core#quote ,real-name))
	  ;; Set up an R7RS environment for the module's body.
	  (import-for-syntax r7rs) ; overwrites "syntax-rules"
	  (import r7rs) ; overwrites existing "import" and "import-for-syntax"
          ,(parse-decls decls)))))
    (_ (syntax-error 'define-library "invalid library definition" form))))

(define (register-r7rs-module name)
  (let ((dummy (string->symbol (conc "\x04r7rs" name))))
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
