(module r7rs

()

(import chicken scheme)
(use srfi-13)

(define (read-asserted-ci-symbol port valid-symbols error-message)
  (let ((sym (##sys#read port ##sys#default-read-info-hook)))
    (or (and (symbol? sym)
             (memq (string->symbol (string-downcase (symbol->string sym))) valid-symbols))
        (##sys#read-error port error-message sym))))

(let ((old-hook ##sys#user-read-hook))
  (set! ##sys#user-read-hook
        (lambda (char port)
          (case char
            ((#\f #\F)
             (read-asserted-ci-symbol port '(f false) "invalid `false' read syntax")
             #f)
            ((#\t #\T)
             (read-asserted-ci-symbol port '(t true) "invalid `true' read syntax")
             #t)
            (else (old-hook))))))

)
