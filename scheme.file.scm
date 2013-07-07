(module scheme.file (call-with-input-file
		     call-with-output-file
		     call-with-input-file
		     delete-file
		     ; TODO open-binary-input-file
		     open-input-file
		     with-input-from-file
		     call-with-output-file
		     file-exists?
		     ; TODO open-binary-output-file
		     open-output-file
		     with-output-to-file)
  (import scheme)
  (import (rename (only chicken delete-file file-exists? :)
		  (file-exists? chicken-file-exists?)))

  ;; CHICKEN's file-exists? returns the filename when true,
  ;; whereas R7RS requires it to return #t or #f.
 
  (: file-exists? (string -> boolean))
  
  (define (file-exists? filename)
    (and (chicken-file-exists? filename) #t))

)
