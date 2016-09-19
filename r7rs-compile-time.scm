;;;; compile-time support code (mostly for modules)


(import matchable)
(use srfi-1 files extras)
(use r7rs-library r7rs-support)

(define (locate-library name loc)		; must be stripped
  ;;XXX scan include-path?
  (let* ((name2 (parse-library-name name loc))
	 (sname2 (symbol->string name2)))
    (or (##sys#find-module name2 #f)
	(memq name2 ##sys#core-library-modules)
	(memq name2 ##sys#core-syntax-modules)
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

(define (current-source-directory)
  (cond (##sys#current-source-filename => pathname-directory)
        (else #f)))

(define (expand/begin e)
  (match (expand e '())
    (('##core#begin . rest)
     (cons '##core#begin
           (map expand/begin rest)))
    (e* e*)))

(define (expand-toplevel-r7rs-library-forms exps)
  (parameterize ((##sys#macro-environment (r7rs-library-macro-environment)))
    (map expand/begin exps)))

(define (read-forms filename ci?)
  (let ((path (##sys#resolve-include-filename filename #t)))
    (fluid-let ((##sys#default-read-info-hook
                 (and (feature? 'compiling) ##compiler#read-info-hook))
                (##sys#include-pathnames
                 (cond ((pathname-directory path) =>
                        (cut cons <> ##sys#include-pathnames))
                       ((current-source-directory) =>
                        (cut cons <> ##sys#include-pathnames))
                       (else ##sys#include-pathnames))))
      (expand-toplevel-r7rs-library-forms
       (parameterize ((case-sensitive (not ci?)))
         (##sys#include-forms-from-file path))))))

(define implicit-r7rs-library-bindings
  '(begin
    cond-expand
    export
    import
    import-for-syntax
    include
    include-ci
    syntax-rules))

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
	      ,(process-include-decls fnames)
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
       `(##core#module
         ,real-name ((,dummy-export))
         ;; gruesome hack: we add a dummy export for adding indirect exports
         (##core#define-syntax ,dummy-export (##core#lambda _ '(##core#undefined)))
         ;; Another gruesome hack: provide feature so "use" works properly
         (##sys#provide (##core#quote ,real-name))
         ;; Set up an R7RS environment for the module's body.
         (import-for-syntax (only r7rs ,@implicit-r7rs-library-bindings))
         (import (only r7rs ,@implicit-r7rs-library-bindings))
         ,(parse-decls decls))))
    (_ (syntax-error 'define-library "invalid library definition" form))))

(define (register-r7rs-module name)
  (let ((dummy (string->symbol (string-append "\x04r7rs" (symbol->string name)))))
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

(define r7rs-define-library
  (er-macro-transformer
   (lambda (x r c)
     (match (strip-syntax x)
       ((_ name decls ...)
        (let ((dummy (register-r7rs-module (parse-library-name name 'define-library))))
          (parse-library-definition x dummy)))
       (else
        (syntax-error 'define-library "invalid library definition" x))))))

(define r7rs-cond-expand
  (er-macro-transformer
   (lambda (x r c)
     (cons (r 'begin)
           (process-cond-expand (cdr x))))))

(define r7rs-include
  (er-macro-transformer
   (lambda (e r c)
     (cons (r 'begin)
           (append-map (cut read-forms <> #f) (cdr e))))))

(define r7rs-include-ci
  (er-macro-transformer
   (lambda (e r c)
     (cons (r 'begin)
           (append-map (cut read-forms <> #t) (cdr e))))))

(define r7rs-import
  (import-transformer 'import))

(define r7rs-import-for-syntax
  (import-transformer 'import-for-syntax))

;; NOTE Not really "r7rs" -- just the core begin rewrapped in
;; a transformer. Used when expanding toplevel library forms.
(define r7rs-begin
  (##sys#make-structure 'transformer (macro-handler 'begin)))

(define (r7rs-library-macro-environment)
  (filter (lambda (p)
            (memv (caddr p)
                  (map (cut ##sys#slot <> 1)
                       (list r7rs-begin
                             r7rs-cond-expand
                             r7rs-define-library
                             r7rs-include
                             r7rs-include-ci))))
          (##sys#macro-environment)))
