;;;
;;; Helpers for working with R7RS library import forms.
;;;
;;; These are used by r7rs-compile-time during library expansion
;;; and scheme.eval for (environment ...) module renaming.
;;;

(module r7rs-library *

  (import scheme chicken matchable)
  (use (only data-structures string-intersperse))

  (define (fixup-import/export-spec spec loc)
    (match spec
      (((and head (or 'only 'except 'rename 'prefix)) name . more)
       (cons head (cons (fixup-import/export-spec name loc) more)))
      ((name ...)
       (parse-library-name name loc))
      ((? symbol? spec) spec)
      (else
       (syntax-error loc "invalid import/export specifier" spec))))

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
      (else (fail)))))
