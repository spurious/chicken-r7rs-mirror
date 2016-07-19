(module scheme.complex ()
  (import scheme)
  (cond-expand
    (no-numbers
     (export angle magnitude imag-part real-part))
    (else
     (import numbers)
     (export angle magnitude make-polar make-rectangular imag-part real-part))))
