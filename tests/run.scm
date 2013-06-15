(use r7rs test)

;; XXX: This seems to be necessary in order to get the syntax-rules
;; from r7rs rather than the built-in CHICKEN one.  I'm not sure if
;; that's correct or not...
(import-for-syntax r7rs)

(define (read-from-string s)
  (with-input-from-string s read))

(test-begin "r7rs tests")

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
               (lambda (e) (raise 'another-error))
               (lambda () (+ 1 (raise 'an-error)))))
  (test "with-exception-handler (raise-continuable)"
        '("should be a number" 65)
        (let* ((exception-object #f)
               (return-value 
                (with-exception-handler
                 (lambda (e) (set! exception-object e) 42)
                 (lambda () (+ (raise-continuable "should be a number") 23)))))
          (list exception-object return-value)))
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
;;
;; XXX TODO: Rewrite in terms of SRFI-6 string port interface, so
;; no call-with-*-string, but use get-output-string and such!
;; Do this when it's clear how to re-export Chicken stuff.
(test-group "string ports"
  (receive (jump-back? jump!)
      (call/cc (lambda (k) (values #f k)))
    (when jump-back? (jump! (void)))
    (let ((string (call-with-output-string
                   (lambda (the-string-port)
                     (receive (one two three)
                         (call-with-port the-string-port
                          (lambda (p)
                            (display "foo" p)
                            ;; Leave the dynamic extent momentarily;
                            ;; jump! will immediately return with #t.
                            (call/cc (lambda (k) (jump! #t k)))
                            (test-assert "Port is still open after excursion"
                                         (output-port-open? the-string-port))
                            (display "bar" p)
                            (values 1 2 3)))
                       (test "call-with-port returns all values yielded by proc"
                             '(1 2 3)
                             (list one two three)))
                     (test-assert "call-with-port closes the port on normal return"
                                  (not (output-port-open? the-string-port)))
                     (test-assert "It's ok to close output ports that are closed"
                                  (close-port the-string-port))
                     (test-error "input-port-open? fails on output ports"
                                 (input-port-open? the-string-port))))))
      (test "call-with-port passes the port correctly and allows temporary escapes"
            "foobar" string)))

  (call-with-input-string "foo"
    (lambda (the-string-port)
      (test-error "output-port-open? fails on input ports"
                  (output-port-open? the-string-port))
      (test-assert "Initially, string port is open"
                   (input-port-open? the-string-port))
      (test "Reading from string delivers the data"
            'foo (read the-string-port))
      (test "After reading all, we get the eof-object"
            (eof-object) (read the-string-port))
      (test-assert "Port is still open after all reads"
                   (input-port-open? the-string-port))
      (close-port the-string-port)
      (test-assert "Port is no longer open after closing it"
                   (not (input-port-open? the-string-port)))
      (test-assert "It's ok to close input ports that are already closed"
                   (close-port the-string-port)))))

;; This is for later. We can't define it inside a group because that
;; would make it locally scoped (as a letrec rewrite), which breaks
;; the syntax-rules underscore tests.  Very subtle (and annoying), this!
(define (_) 'underscore-procedure)
(define ___ 'triple-underscore-literal)

(test-group "syntax-rules"
  (test "let-syntax w/ basic syntax-rules"
        100
        (let-syntax ((foo (syntax-rules ()
                            ((_ x form)
                             (let ((tmp x))
                               (if (number? tmp)
                                   form
                                   (error "not a number" tmp)))))))
          (foo 2 100)))
  (let-syntax ((foo (syntax-rules ()
                      ((_ #(a ...)) (list a ...)))))
    (test "Basic matching of vectors"
          '(1 2 3) (foo #(1 2 3))))
  ;; ellipsis pattern element wasn't matched - reported by Jim Ursetto (fixed rev. 13582)
  (let-syntax ((foo (syntax-rules ()
                      ((_ (a b) ...)
                       (list 'first '(a b) ...))
                      ((_ a ...)
                       (list 'second '(a) ...)))))
    (test "Basic ellipsis match"
          '(first (1 2) (3 4) (5 6)) (foo (1 2) (3 4) (5 6)))
    (test "Ellipsis match of length 1 does not match length 2"
          '(second (1)) (foo 1))
    (test "Ellipsis match of lists with mismatched lengths (used to fail)"
          '(second ((1 2)) ((3)) ((5 6))) (foo (1 2) (3) (5 6))))

  (test "letrec-syntax"
        34
        (letrec-syntax ((foo (syntax-rules () ((_ x) (bar x))))
                        (bar (syntax-rules () ((_ x) (+ x 1)))))
          (foo 33)))
  (test "Basic hygienic rename of syntactic keywords"
        'now
        (let-syntax ((when (syntax-rules ()
                             ((when test stmt1 stmt2 ...)
                              (if test
                                  (begin stmt1
                                         stmt2 ...))))))
          (let ((if #t))
            (when if (set! if 'now))
            if)))
  (test "Basic hygienic rename of shadowed outer let"
        'outer
        (let ((x 'outer))
          (let-syntax ((m (syntax-rules () ((m) x))))
            (let ((x 'inner))
              (m)))))
  (test "Simple recursive letrec expansion"
        7
        (letrec-syntax
            ((my-or (syntax-rules ()
                      ((my-or) #f)
                      ((my-or e) e)
                      ((my-or e1 e2 ...)
                       (let ((temp e1))
                         (if temp
                             temp
                             (my-or e2 ...)))))))
          (let ((x #f)
                (y 7)
                (temp 8)
                (let odd?)
                (if even?))
            (my-or x
                   (let temp)
                   (if y)
                   y))))
  ;; From Al* Petrofsky's "An Advanced Syntax-Rules Primer for the Mildly Insane"
  (let ((a 1))
    (letrec-syntax
        ((foo (syntax-rules ()
                ((_ b)
                 (bar a b))))
         (bar (syntax-rules ()
                ((_ c d)
                 (cons c (let ((c 3))
                           (list d c 'c)))))))
      (let ((a 2))
        (test "Al* Petrofsky torture test" '(1 2 3 a) (foo a)))))
  (let-syntax
      ((foo (syntax-rules ()
              ((_)
               '#(b)))))
    (test "Quoted symbols inside vectors are stripped of syntactic info"
          '#(b) (foo)))
  (let-syntax ((kw (syntax-rules (baz)
                     ((_ baz) "baz")
                     ((_ any) "no baz"))))
    (test "syntax-rules keywords match" "baz" (kw baz))
    (test "syntax-rules keywords no match" "no baz" (kw xxx))
    (let ((baz 100))
      (test "keyword loses meaning if shadowed" "no baz" (kw baz))))
  (test "keyword also loses meaning for builtins (from R7RS section 4.3.2)"
        'ok
        (let ((=> #f))
          (cond (#t => 'ok))))
  (test "Nested identifier shadowing works correctly"
        '(3 4)
        (let ((foo 3))
          (let-syntax ((bar (syntax-rules () ((_ x) (list foo x)))))
            (let ((foo 4))
              (bar foo)))))
  (let-syntax ((c (syntax-rules ()
                    ((_)
                     (let ((x 10))
                       (let-syntax ((z (syntax-rules ()
                                         ((_) (quote x)))))
                         (z))))))
               (c2 (syntax-rules ()
                     ((_)
                      (let ((x 10))
                        (let-syntax
                            ((z (syntax-rules ()
                                  ((_) (let-syntax
                                           ((w (syntax-rules ()
                                                 ((_) (quote x)))))
                                         (w))))))
                          (z)))))))
    ;; Reported by Matthew Flatt
    (test "strip-syntax cuts across three levels of syntax"
          "x" (symbol->string (c)))
    (test "strip-syntax cuts across four levels of syntax"
          "x" (symbol->string (c2))))
  (let-syntax ((foo (syntax-rules 
                        ___ () 
                        ((_ vals ___) (list '... vals ___)))))
    (test "Alternative ellipsis (from SRFI-46)"
          '(... 1 2 3) (foo 1 2 3)))
  (let-syntax ((let-alias (syntax-rules
                              ___ ()
                              ((_ new old code ___)
                               (let-syntax
                                   ((new
                                     (syntax-rules ()
                                       ((_ args ...) (old args ...)))))
                                 code ___)))))
    (let-alias inc (lambda (x) (+ 1 x))
               (test "Ellipsis rules are reset in new macro expansion phase"
                     3 (inc 2))))
  (let-syntax ((foo (syntax-rules ()
                      ((_ (a ... b) ... (c d))
                       (list (list (list a ...) ... b ...) c d))
                      ((_ #(a ... b) ... #(c d) #(e f))
                       (list (list (vector a ...) ... b ...) c d e f))
                      ((_ #(a ... b) ... #(c d))
                       (list (list (vector a ...) ... b ...) c d)))))
    (test-group "rest patterns after ellipsis (SRFI-46 smoke test)"
      (test '(() 1 2) (foo (1 2)))
      (test '(((1) 2) 3 4) (foo (1 2) (3 4)))
      (test '(((1 2) (4) 3 5) 6 7)
            (foo (1 2 3) (4 5) (6 7)))
      (test '(() 1 2)
            (foo #(1 2)))
      (test '((#() 1) 2 3)
            (foo #(1) #(2 3)))
      (test '((#(1 2) 3) 4 5)
            (foo #(1 2 3) #(4 5)))
      (test '((#(1 2) 3) 4 5 6 7)
            (foo #(1 2 3) #(4 5) #(6 7)))
      (test '(() 1 2 3 4)
            (foo #(1 2) #(3 4)))
      (test '((#(1) 2) 3 4 5 6)
            (foo #(1 2) #(3 4) #(5 6)))
      (test '((#(1 2) #(4) 3 5) 6 7 8 9)
            (foo #(1 2 3) #(4 5) #(6 7) #(8 9)))))
  (let-syntax ((foo (syntax-rules ()
                      ((_ #((a) ...)) (list a ...)))))
    (test "Bug discovered during implementation of rest patterns"
          '(1)
          (foo #((1)))))
  ;; R7RS: (<ellipsis> <template>) is like <template>, ignoring
  ;; occurrances of <ellipsis> inside the template.
  (let-syntax ((be-like-begin
                (syntax-rules ()
                  ((be-like-begin name)
                   (define-syntax name
                     (syntax-rules ()
                       ((name expr (... ...))
                        (begin expr (... ...)))))))))
    (be-like-begin sequence)
    (test "be-like-begin from R7RS 4.3.2 (nested ellipsis are not expanded)"
          4 (sequence 1 2 3 4)))
  (let-syntax ((ignore-underscores
                (syntax-rules ()
                  ((_ _ _ _) (_)))))
    (test "underscores are ignored in patterns"
          'underscore-procedure (ignore-underscores _ b c)))

  (test-group "undefined behaviours: mixing keywords, ellipsis and underscores"
    (test-group "underscore as keyword literal"
      (define-syntax match-literal-underscores ; for eval
        (syntax-rules (_)
          ((x a _ c) (_))
          ((x _ b c) 1)))
      (test-error "Missing literal underscore keyword causes syntax-error"
                  (eval '(match-literal-underscores d e f)))
      (test "Literal underscore matches"
            1 (match-literal-underscores _ h i))
      (test "Literal underscore matches even if it refers to toplevel binding"
            'underscore-procedure (match-literal-underscores g _ i)))
    
    (test-group "underscore as ellipsis"
     ;; It's undefined what this should do.  Logically, it should be
     ;; possible to bind _ as an ellipsis identifier.
     (define-syntax match-ellipsis-underscores ; for eval
       (syntax-rules _ () ((x a _ c) (list a _ c))))
     (test-error "No rule matching if prefix is omitted"
                 (eval '(match-ellipsis-underscores)))
     (test "Only prefix is supplied"
           '(1) (match-ellipsis-underscores 1))
     (test "Ellipsis does its work if multiple arguments given"
           '(1 2 3 4 5 6) (match-ellipsis-underscores 1 2 3 4 5 6)))

    (test-group "underscore as ellipsis mixed with underscore literal"
      ;; Even more undefined behaviour: mixing literals and ellipsis identifiers
      ;; Currently, ellipsis identifiers have precedence over the other two.
      (define-syntax match-ellipsis-and-literals-underscores ; for eval
        (syntax-rules _ (_) ((x a _ c) (list a _ c))))
      (test-error "No rule matching if prefix is omitted"
                  (eval '(match-ellipsis-and-literals-underscores)))
      (test '(1) (match-ellipsis-and-literals-underscores 1))
      (test '(1 2 3) (match-ellipsis-and-literals-underscores 1 2 3))
      (test '(1 2 3 4 5 6) (match-ellipsis-and-literals-underscores 1 2 3 4 5 6)))

    (test-group "\"custom\" ellipsis and literal of the same identifier"
      ;; This is similar to the above, but maybe a little simpler because
      ;; it does not use reserved names:
      (define-syntax match-ellipsis-literals
        (syntax-rules ___ (___)
                      ((_ x ___) (list x ___))))
      (test "Ellipsis as literals"
            '(1) (match-ellipsis-literals 1))
      (test "Ellipsis as literals multiple args"
            '(1 2) (match-ellipsis-literals 1 2))
      (test "Toplevel binding of the same name as ellipsis"
            '(1 triple-underscore-literal) (match-ellipsis-literals 1 ___))))

  (letrec-syntax ((usetmp
                   (syntax-rules ()
                     ((_ var) 
                      (list var))))
                  (withtmp
                   (syntax-rules ()
                     ((_ val exp)
                      (let ((tmp val))
                        (exp tmp))))))
    (test "Passing a macro as argument to macro"
          '(99)
          (withtmp 99 usetmp)))

  ;; renaming of keyword argument (#277)
  (let-syntax ((let-hello-proc
                (syntax-rules ()
                  ((_ procname code ...)
                   (let ((procname (lambda (#!key (who "world"))
                                     (string-append "hello, " who))))
                     code ...)))))
    (let-hello-proc bar
         ;; This is not R7RS, but R7RS should not interfere with other
         ;; CHICKEN features!
         (test "DSSSL keyword arguments aren't renamed (not R7RS)"
               "hello, XXX" (bar who: "XXX")))))

(test-end "r7rs tests")

(test-exit)
