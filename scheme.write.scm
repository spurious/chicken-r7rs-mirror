(module scheme.write (display
		      write
		      ; write-shared
		      write-simple)
  (import scheme)
  (define write-simple write))
