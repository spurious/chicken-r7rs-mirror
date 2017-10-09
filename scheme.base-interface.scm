(export
  * + - /
  <= < >= = >
  abs 
  and
  append
  apply
  assoc assq assv
  begin
  binary-port?
  boolean? boolean=?
  bytevector
  bytevector-append 
  bytevector-copy bytevector-copy!
  bytevector-length bytevector-u8-ref bytevector-u8-set!
  bytevector?
  car cdr
  caar cadr cdar cddr
  call-with-current-continuation call/cc
  call-with-port 
  call-with-values
  case
  ceiling
  char-ready?
  char->integer integer->char
  char=? char<? char>? char<=? char>=?
  char?
  close-input-port close-output-port
  close-port
  complex?
  cond
  cond-expand
  cons
  current-input-port current-output-port current-error-port
  define
  define-record-type
  define-syntax
  define-values
  denominator numerator
  do
  dynamic-wind
  eof-object
  eof-object?
  eq? eqv? equal?
  error
  error-object-irritants error-object-message
  error-object?
  even? odd?
  exact inexact
  #|
  exact-integer-sqrt
  exact-integer?
  |#
  exact? inexact?
  exp
  expt
  features
  file-error?
  floor
  #|
  floor/ floor-quotient floor-remainder
  |#
  flush-output-port
  for-each
  gcd lcm
  get-output-bytevector
  get-output-string
  guard
  if
  #|
  import ; provided by the "r7rs" module
  import-for-syntax ; same
  |#
  include 
  include-ci
  input-port-open? output-port-open?
  input-port? output-port?
  integer?
  lambda
  length
  let let*
  letrec letrec*
  let-values let*-values
  let-syntax letrec-syntax
  list list-copy list-ref list-set! list-tail list?
  list->vector
  make-bytevector
  make-list
  make-parameter
  make-string
  make-vector
  map
  max min
  member memq memv
  modulo remainder
  negative? positive?
  newline
  not
  null?
  number->string string->number
  number?
  open-input-bytevector open-output-bytevector
  open-input-string open-output-string
  or
  pair?
  parameterize
  peek-char
  peek-u8
  port?
  procedure?
  quasiquote
  quote
  quotient remainder
  raise raise-continuable
  rational?
  #|
  rationalize
  |#
  read-bytevector read-bytevector!
  read-char
  read-error?
  read-line
  read-string
  read-u8
  real?
  reverse
  round
  set!
  set-car! set-cdr!
  square
  string
  string->list list->string
  string->utf8 utf8->string
  string->symbol symbol->string
  string->vector
  string-append
  string-copy
  string-copy!
  string-fill!
  string-for-each
  string-length
  string-map
  string-ref string-set!
  string=? string<? string>? string<=? string>=?
  string?
  substring
  symbol=?
  symbol?
  syntax-error
  #|
  syntax-rules ; provided by the "r7rs" module
  |#
  textual-port?
  truncate
  #|
  truncate/ truncate-quotient truncate-remainder
  |#
  u8-ready?
  unless
  #|
  unquote unquote-splicing ; provided by `quasiquote`
  |#
  values
  vector
  vector-append
  vector-copy vector-copy!
  vector-fill!
  vector-for-each
  vector-length
  vector-map
  vector-ref vector-set!
  vector->list
  vector->string
  vector?
  when
  with-exception-handler
  write-bytevector 
  write-char
  write-string
  write-u8
  zero?
  )
