(module scheme.r5rs ()
  (import (rename scheme
                  (null-environment %null-environment)
                  (scheme-report-environment %scheme-report-environment)))
  (import chicken.base chicken.module chicken.syntax chicken.type)
  (import scheme.eval)

  (export angle make-polar make-rectangular rationalize)
  (export null-environment scheme-report-environment)

  (reexport
    (except scheme
            null-environment scheme-report-environment eval
            and begin case cond define define-syntax delay do
            if lambda let let* let-syntax letrec letrec-syntax
            or quasiquote quote set! syntax-rules))

  (define-constant null-environment-identifiers
    '(and begin case cond cond-expand define define-syntax delay
      delay-force do if lambda let let* let-syntax letrec letrec*
      letrec-syntax or quasiquote quote set! syntax-rules))

  (: null-environment (fixnum -> (struct environment)))

  (define (null-environment version)
    (case version
      ((7)  (environment `(only (scheme base) ,@null-environment-identifiers)))
      ((5)  (environment `(only (scheme r5rs) ,@null-environment-identifiers)))
      (else (%null-environment version))))

  (: scheme-report-environment (fixnum -> (struct environment)))

  (define (scheme-report-environment version)
    (case version
      ((7)  (environment '(scheme base)))
      ((5)  (environment '(scheme r5rs)))
      (else (%scheme-report-environment version)))))
