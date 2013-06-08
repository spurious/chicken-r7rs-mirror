(use r7rs test)

(define (read-from-string s)
  (with-input-from-string s read))

(test-group "long boolean literals"
 (test #t (read-from-string "#t"))
 (test #f (read-from-string "#f"))
 (test #t (read-from-string "#true"))
 (test #f (read-from-string "#false"))
 (test-error (read-from-string "#faux")))

(define-syntax catch
  (syntax-rules ()
    ((_ . body) (handle-exceptions e e . body))))

(test-group "exceptions"
  (test "with-exception-handler (escape)"
        'exception
        (call-with-current-continuation
         (lambda (k)
           (with-exception-handler
            (lambda (e) (k 'exception))
            (lambda () (+ 1 (raise 'an-error)))))))
  (test-error "with-exception-handler (return)"
              (with-exception-handler
               (lambda (e) 'ignore)
               (lambda () (+ 1 (raise 'an-error)))))
  (test-error "with-exception-handler (raise)"
              (with-exception-handler
               (lambda (e) 'ignore)
               (lambda () (+ 1 (raise 'an-error)))))
  (test "with-exception-handler (raise-continuable)"
        65
        (with-exception-handler
         (lambda (e) 42)
         (lambda () (+ (raise-continuable "should be a number") 23))))
  (test "error-object? (#f)" #f (error-object? 'no))
  (test "error-object? (#t)" #t (error-object? (catch (car '()))))
  (test "error-object-message" "fubar" (error-object-message (catch (error "fubar"))))
  (test "error-object-irritants" '(42) (error-object-irritants (catch (error "fubar" 42))))
  (test "read-error? (#f)" #f (read-error? (catch (car '()))))
  (test "read-error? (#t)" #t (read-error? (catch (read-from-string ")"))))
  (test "file-error? (#f)" #f (file-error? (catch (car '()))))
  (test "file-error? (#t)" #t (file-error? (catch (open-input-file "foo"))))
  (test-error "guard (no match)"
              (guard (condition ((assq 'c condition))) (raise '((a . 42)))))
  (test "guard (match)"
        '(b . 23)
        (guard (condition ((assq 'b condition))) (raise '((b . 23)))))
  (test "guard (=>)"
        42
        (guard (condition ((assq 'a condition) => cdr)) (raise '((a . 42)))))
  (test "guard (multiple)"
        '(b . 23)
        (guard (condition
                ((assq 'a condition) => cdr)
                ((assq 'b condition)))
               (raise '((b . 23))))))

;; call-with-port is not supposed to close its port when leaving the
;; dynamic extent, only on normal return.
(test-group "ports"
  (let*-values (((jump-back? jump!) (call/cc (lambda (k) (values #f k))))
                (_ (and jump-back? (jump! (void))))
                ((string)
                 (call-with-output-string
                  (lambda (the-string-port)
                    (test "call-with-port returns all values yielded by proc"
                          '(1 2 3)
                          (receive (call-with-port
                                    the-string-port
                                    (lambda (p)
                                      (display "foo" p)
                                      ;; Leave the dynamic extent momentarily;
                                      ;; jump! will immediately return with #t.
                                      (call/cc (lambda (k) (jump! #t k)))
                                      (display "bar" p)
                                      (values 1 2 3)))))
                    (test-assert "call-with-port closes the port on normal return"
                                 (port-closed? the-string-port))))))
    (test "call-with-port passes the port correctly and allows temporary escapes"
          "foobar" string)))
