(define-library (r2pnl builtin convert-func)
  (export convert-func-dict)
  (import (scheme base)
          (r2pnl values)
          (r2pnl interpreter)
          (r2pnl parser))
  (begin
    (define (->string-func interp)
      (let* ((v (stack-pop! interp)))
        (stack-push! interp (value->display-string v))));displayの方がいい気がしてきた

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

    (define (symbol->string-func interp);string経由で変なsymbolも作れちゃうのは内緒
      (let* ((sym (stack-pop! interp)))
        (if (symbol-value? sym)
            (stack-push! interp (symbol-value-token sym))
            (interp-error! interp "TypeError" "the symbol->string func expects one symbol-value"))))

    (define (char->integer-func interp)
      (let* ((ch (stack-pop! interp)))
        (if (and (string? ch) (= 1 (string-length ch)))
            (stack-push! interp (char->integer (string-ref ch 0)))
            (interp-error! interp "TypeError" "the \"char->integer\" func expects a single-character string."))))

    (define (integer->char-func interp)
      (let* ((i (stack-pop! interp)))
        (if (and (number? i) (not (negative? i)))
            (stack-push! interp (string (integer->char (exact i))))
            (interp-error! interp "TypeError" "the \"char->integer\" func expects a non-negative integer."))))

    (define (string->list-func interp)
      (let* ((s (stack-pop! interp)))
        (if (string? s)
            (stack-push! interp
                         (make-block-value
                          (map
                           (lambda (c) (cons (string c) (interp-token-line interp)))
                           (string->list s))))
            (interp-error! interp "TypeError" "the \"string->list\" func expects a string" ))))

    (define (quote-func interp)
      (stack-push! interp (make-quote-value (stack-pop! interp))))

    (define (unquote-func interp)
      (let* ((quo (stack-pop! interp)))
        (if (not (quote-value? quo))
            (interp-error! interp "TypeError" "the \"unquote\" func expects a quote")
            (stack-push! interp (parse-one-token (quote-value-token quo) (interp-token-line interp))))))

    (define convert-func-dict
      `(("->string" . ,->string-func)
        ("string->number" . ,string->number-func)
        ("string->symbol" . ,string->symbol-func)
        ("symbol->string" . ,symbol->string-func)
        ("char->integer" . ,char->integer-func)
        ("integer->char" . ,integer->char-func)
        ("string->list" . ,string->list-func)
        ("quote" . ,quote-func)
        ("unquote" . ,unquote-func)))))
