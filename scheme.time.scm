(module scheme.time (current-second
		     current-jiffy
		     jiffies-per-second)
  (import (only chicken.base define-constant)
	  (only chicken.time current-seconds current-milliseconds)
	  (only chicken.type :)
	  (only scheme + define inexact->exact))

  ;; As of 2012-06-30.
  (define-constant tai-offset 35.)

  (: current-second (--> float))
  (define (current-second) (+ (current-seconds) tai-offset))

  (: current-jiffy (--> fixnum))
  (define (current-jiffy) (inexact->exact (current-milliseconds)))

  (: jiffies-per-second (--> fixnum))
  (define (jiffies-per-second) 1000))
