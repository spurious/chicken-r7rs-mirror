(use r7rs test)

(define (read-from-string s)
  (with-input-from-string s read))

(test-group "long boolean literalsa"
 (test #t (read-from-string "#t"))
 (test #f (read-from-string "#f"))
 (test #t (read-from-string "#true"))
 (test #f (read-from-string "#false"))
 (test-error (read-from-string "#faux")))

(define-syntax catch
  (syntax-rules ()
    ((_ . body) (handle-exceptions e e . body))))

(test-group "exceptions"
 (test "error-object? (#f)" #f (error-object? 'no))
 (test "error-object? (#t)" #t (error-object? (catch (car '()))))
 (test "error-object-message" "fubar" (error-object-message (catch (error "fubar"))))
 (test "error-object-irritants" '(42) (error-object-irritants (catch (error "fubar" 42))))
 (test "read-error? (#f)" #f (read-error? (catch (car '()))))
 (test "read-error? (#t)" #t (read-error? (catch (read-from-string ")"))))
 (test "file-error? (#f)" #f (file-error? (catch (car '()))))
 (test "file-error? (#t)" #t (file-error? (catch (open-input-file "foo")))))
