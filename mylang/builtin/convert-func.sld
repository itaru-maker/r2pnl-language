(define-library (mylang builtin convert-func)
  (export convert-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang interpreter))
  (begin
    (define (to-string-func interp)
      (let* ((v (stack-pop! interp)))
        (stack-push! interp (value->write-string v))));writeの方でいいのかは不明

    (define (string->number-func interp)
      (let* ((str (stack-pop! interp)))
        (if (string? str)
            (if (string->number str)
                (stack-push! interp (string->number str))
                (stack-push! interp #f))
            (interp-error! interp "TypeError" "the string->number func expects one string."))))

    (define (string->symbol-func interp)
      (let* ((str (stack-pop! interp)))
        (if (string? str)
            (stack-push! interp (make-symbol-value str))
            (interp-error! interp "TypeError" "the string->symbol func expects one string."))))

    (define (symbol->string-func interp)
      (let* ((sym (stack-pop! interp)))
        (if (symbol-value? sym)
            (stack-push! interp (symbol-value-token sym))
            (interp-error! interp "TypeError" "the symbol->string func expects one symbol-value"))))

    (define convert-func-dict
      `(("to-string" . ,to-string-func)
        ("string->number" . ,string->number-func)
        ("string->symbol" . ,string->symbol-func)
        ("symbol->string" . ,symbol->string-func)))))
