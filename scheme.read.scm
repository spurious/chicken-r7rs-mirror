(module scheme.read (read)
  (import (except scheme read)
	  (only chicken current-read-table fx+ fx= optional unless when))

  (define (data? o)
    (not (procedure? o)))

  (define (unthunk o fail)
    (let ((v (o)))
      (cond ((data? v) v)
	    ((eq? v o)
	     (fail "self-referential datum"))
	    (else
	     (unthunk v fail)))))

  ;; Fills holes in `o` destructively.
  (define (unthunkify! o fail)
    (let loop! ((o o))
      (cond ((pair? o)
	     (if (data? (car o))
		 (loop! (car o))
		 (set-car! o (unthunk (car o) fail)))
	     (if (data? (cdr o))
		 (loop! (cdr o))
		 (set-cdr! o (unthunk (cdr o) fail))))
	    ((vector? o)
	     (let ((len (vector-length o)))
	       (do ((i 0 (fx+ i 1)))
		   ((fx= i len))
		 (let ((v (vector-ref o i)))
		   (if (data? v)
		       (loop! v)
		       (vector-set! o i (unthunk v fail))))))))))

  (define (read-with-shared-structure port)

    (define read-table (current-read-table))
    (unless (##sys#slot read-table 3)
      (##sys#setslot read-table 3 (##sys#make-vector 256 #f)))

    (define read-hash/orig  (##sys#slot (##sys#slot read-table 3) 35))
    (define read-equal/orig (##sys#slot (##sys#slot read-table 3) 61))

    (define shared '())
    (define (register-shared! n thunk)
      (set! shared (cons (cons n thunk) shared)))

    (define (read-hash/shared _ p n)
      (##sys#read-char-0 p)
      (cond ((assv n shared) => cdr)
	    (else (##sys#read-error p "undefined datum" n))))

    (define (read-equal/shared _ p n)
      (##sys#read-char-0 p)
      (letrec ((o (begin
		    (register-shared! n (lambda () o))
		    (##sys#read p ##sys#default-read-info-hook))))
	o))

    (define (read/shared p)
      (let ((o (##sys#read port ##sys#default-read-info-hook)))
	 (when (pair? shared)
	   (unthunkify! o (lambda a (apply ##sys#read-error p a))))
	 o))

    (dynamic-wind
     (lambda ()
       (##sys#setslot (##sys#slot read-table 3) 35 read-hash/shared)
       (##sys#setslot (##sys#slot read-table 3) 61 read-equal/shared))
     (lambda ()
       (read/shared port))
     (lambda ()
       (##sys#setslot (##sys#slot read-table 3) 35 read-hash/orig)
       (##sys#setslot (##sys#slot read-table 3) 61 read-equal/orig))))

  (: read (#!optional input-port -> *))
  (define (read . port)
    (read-with-shared-structure
     (optional port (current-input-port)))))
