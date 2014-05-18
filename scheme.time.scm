(module scheme.time (current-second
		     current-jiffy
		     jiffies-per-second)
  (import (only scheme define inexact->exact)
	  (rename (only chicken current-seconds current-milliseconds :)
		  (current-seconds current-second)))

  (: current-jiffy (--> fixnum))
  (define (current-jiffy) (inexact->exact (current-milliseconds)))

  (: jiffies-per-second (--> fixnum))
  (define (jiffies-per-second) 1000))
