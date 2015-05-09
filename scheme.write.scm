(module scheme.write (display
		      write
		      write-shared
		      write-simple)
  (import (rename scheme (display display-simple) (write write-simple))
	  (only chicken : feature? foldl fx+ fx= fx<= optional when))

  (when (feature? 'csi)
    (set! ##sys#repl-print-hook
      (lambda (o p)
        (write o p)
        (newline))))

  (define (interesting? o)
    (or (pair? o)
	(and (vector? o)
	     (fx<= 1 (vector-length o)))))

  (define (uninteresting? o)
    (not (interesting? o)))

  (define (display-char c p)
    ((##sys#slot (##sys#slot p 2) 2) p c))

  (define (display-string s p)
    ((##sys#slot (##sys#slot p 2) 3) p s))

  ;; Build an alist mapping `interesting?` objects to boolean values
  ;; indicating whether those objects occur shared in `o`.
  (define (find-shared o cycles-only?)

    (define seen '())
    (define (seen? x) (assq x seen))
    (define (seen! x) (set! seen (cons (cons x 1) seen)))

    ;; Walk the form, tallying the number of times each object is
    ;; encountered. This has the effect of filling `seen` with
    ;; occurence counts for all objects satisfying `interesting?`.
    (let walk! ((o o))
      (when (interesting? o)
	(cond ((seen? o) =>
	       (lambda (p)
		 (set-cdr! p (fx+ (cdr p) 1))))
	      ((pair? o)
	       (seen! o)
	       (walk! (car o))
	       (walk! (cdr o)))
	      ((vector? o)
	       (seen! o)
	       (let ((len (vector-length o)))
		 (do ((i 0 (fx+ i 1)))
		     ((fx= i len))
		   (walk! (vector-ref o i))))))
	;; If we're only interested in cycles and this object isn't
	;; self-referential, discount it (resulting in `write` rather
	;; than `write-shared` behavior).
	(when cycles-only?
	  (let ((p (seen? o)))
	    (when (fx<= (cdr p) 1)
	      (set-cdr! p 0))))))

    ;; Mark shared objects #t, unshared objects #f.
    (foldl (lambda (a p)
	     (if (fx<= (cdr p) 1)
		 (cons (cons (car p) #f) a)
		 (cons (cons (car p) #t) a)))
	   '()
	   seen))

  (define (write-with-shared-structure writer obj cycles-only? port)

    (define label 0)
    (define (assign-label! pair)
      (set-cdr! pair label)
      (set! label (fx+ label 1)))

    (define shared
      (find-shared obj cycles-only?))

    (define (write-interesting/shared o)
      (cond ((pair? o)
	     (display-char #\( port)
	     (write/shared (car o))
	     (let loop ((o (cdr o)))
	       (cond ((null? o)
		      (display-char #\) port))
		     ((and (pair? o)
			   (not (cdr (assq o shared))))
		      (display-char #\space port)
		      (write/shared (car o))
		      (loop (cdr o)))
		     (else
		      (display-string " . " port)
		      (write/shared o)
		      (display-char #\) port)))))
	    ((vector? o)
	     (display-string "#(" port)
	     (write/shared (vector-ref o 0))
	     (let ((len (vector-length o)))
	       (do ((i 1 (fx+ i 1)))
		   ((fx= i len)
		    (display-char #\) port))
		 (display-char #\space port)
		 (write/shared (vector-ref o i)))))))

    (define (write/shared o)
      (if (uninteresting? o)
	  (writer o port)
	  (let* ((p (assq o shared))
		 (d (cdr p)))
	    (cond ((not d)
		   (write-interesting/shared o))
		  ((number? d)
		   (display-char #\# port)
		   (writer d port)
		   (display-char #\# port))
		  (else
		   (display-char #\# port)
		   (writer label port)
		   (display-char #\= port)
		   (assign-label! p)
		   (write-interesting/shared o))))))

    (write/shared obj))

  (: display (* #!optional output-port -> undefined))
  (define (display o . p)
    (write-with-shared-structure
     display-simple
     o
     #t
     (optional p (current-output-port))))

  (: write (* #!optional output-port -> undefined))
  (define (write o . p)
    (write-with-shared-structure
     write-simple
     o
     #t
     (optional p (current-output-port))))

  (: write-shared (* #!optional output-port -> undefined))
  (define (write-shared o . p)
    (write-with-shared-structure
     write-simple
     o
     #f
     (optional p (current-output-port)))))
