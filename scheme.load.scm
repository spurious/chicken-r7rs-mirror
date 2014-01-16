(module scheme.load (load)
  (import chicken)
  (import (rename scheme (load %load)
			 (eval %eval)))

  (: load (string #!optional (struct environment) -> undefined))

  (define load
    (case-lambda
      ((filename)
       (%load filename))
      ((filename environment)
       (%load filename (lambda (exp)
			 (%eval exp environment)))))))
