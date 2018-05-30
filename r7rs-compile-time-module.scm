(module r7rs-compile-time (r7rs-begin
                           r7rs-cond-expand
                           r7rs-define-library
                           r7rs-include
                           r7rs-include-ci)
  (import scheme chicken.base)
  (include "r7rs-compile-time.scm"))
