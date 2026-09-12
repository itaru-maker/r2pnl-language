(define-library (mylang tokens)
  (export
   semicolon? the-semicolon
   r-paren? the-r-paren
   l-paren? the-l-paren)
  (import (scheme base))

  (begin
    (define-record-type <semicolon>
      (make-semicolon)
      semicolon?)

    (define the-semicolon (make-semicolon))

    (define-record-type <r-paren>
      (make-r-paren)
      r-paren?)

    (define the-r-paren (make-r-paren))
  
    (define-record-type <l-paren>
      (make-l-paren)
      l-paren?)

    (define the-l-paren (make-l-paren))))
