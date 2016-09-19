(module scheme.inexact ()
  (import scheme)
  (cond-expand
    (no-numbers
     (import chicken)
     (export acos asin atan exp sin cos tan finite? log sqrt))
    (else
     (import numbers)
     (export acos asin atan exp infinite? sin cos tan finite? log sqrt nan?))))
