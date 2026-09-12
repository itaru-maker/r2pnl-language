(define-library (mylang tokens)
  (export
   r-paren? the-r-paren
   l-paren? the-l-paren
   list-marker? the-list-marker)
  (import (scheme base))

  (begin
    (define-record-type <r-paren>
      (make-r-paren)
      r-paren?)

    (define the-r-paren (make-r-paren))
  
    (define-record-type <l-paren>
      (make-l-paren)
      l-paren?)

    (define the-l-paren (make-l-paren))

    (define-record-type <list-marker>
      (make-list-marker)
      list-marker?)

    (define the-list-marker (make-list-marker))))
