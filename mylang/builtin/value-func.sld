(define-library (mylang builtin value-func)
  (export value-func-dict)
  (import (scheme base)
          (scheme write)
          (mylang values)
          (mylang tokens)
          (mylang interpreter))

  (begin
    (define (number?-func interp)
       (stack-push! interp (number? (stack-pop! interp))))

    (define (string?-func interp)
      (stack-push! interp (string? (stack-pop! interp))))

    (define (true?-func interp)
      (stack-push! interp (eq? #t (stack-pop! interp))))

    (define (false?-func interp)
      (stack-push! interp (eq? #f (stack-pop! interp))))

    (define (nil?-func interp)
      (stack-push! interp (eq? the-nil (stack-pop! interp))))

    (define (symbol?-func interp)
      (stack-push! interp (symbol-value? (stack-pop! interp))))

    (define (lazy?-func interp)
      (stack-push! interp (lazy-value? (stack-pop! interp))))

    (define (block?-func interp)
      (stack-push! interp (block-value? (stack-pop! interp))))

    (define (builtin-func?-func interp)
      (stack-push! interp (builtin-func? (stack-pop! interp))))

    (define (lambda?-func interp)
      (stack-push! interp (lambda-value? (stack-pop! interp))))

    (define (r-paren?-func interp)
      (stack-push! interp (eq? the-r-paren (stack-pop! interp))))
    
    (define (l-paren?-func interp)
      (stack-push! interp (eq? the-l-paren (stack-pop! interp))))

    (define value-func-dict
      `(("number?" . ,number?-func)
        ("string?" . ,string?-func)
        ("true?" . ,true?-func)
        ("false?" . ,false?-func)
        ("nil?" . ,nil?-func)
        ("symbol?" .,symbol?-func)
        ("lazy?" . ,lazy?-func)
        ("block?" . ,block?-func)
        ("builtin?" . ,builtin-func?-func)
        ("lambda?" . ,lambda?-func)
        ("r-paren?" . ,r-paren?-func)
        ("l-paren?" . ,l-paren?-func)))))
