(module scheme.time (current-second
		     current-jiffy
		     jiffies-per-second)
  (import scheme)
  (import (rename chicken (current-seconds current-second)))
  ;; sic, XXX, TODO, etc.
  (define current-jiffy current-second)
  (define (jiffies-per-second) 1))
