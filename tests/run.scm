(use r7rs test)

(define (read-from-string s)
  (with-input-from-string s read))

(test-group "long boolean literalsa"
 (test #t (read-from-string "#t"))
 (test #f (read-from-string "#f"))
 (test #t (read-from-string "#true"))
 (test #f (read-from-string "#false"))
 (test-error (read-from-string "#faux")))
