(module scheme.time (current-second
		     current-jiffy
		     jiffies-per-second)
  (import (only scheme define inexact->exact)
	  (only chicken : define-constant current-seconds current-milliseconds fp+))

  ;; As of 2012-06-30.
  (define-constant tai-offset 35.)

  (: current-second (--> float))
  (define (current-second) (fp+ (current-seconds) tai-offset))

  (: current-jiffy (--> fixnum))
  (define (current-jiffy) (inexact->exact (current-milliseconds)))

  (: jiffies-per-second (--> fixnum))
  (define (jiffies-per-second) 1000))
