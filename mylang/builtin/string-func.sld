(define-library (mylang builtin string-func)
  (export string-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang interpreter))

  (begin
    (define (string-concat-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (if (not (and (string? b) (string? a)))
            (interp-error! interp "TypeError" "The \"string-concat\" func expects two string")
            (stack-push! interp (string-append b a)))))

    (define (string-len-func interp)
      (let* ((a (stack-pop! interp)))
        (if (string? a)
            (stack-push! interp (string-length a))
            (interp-error! interp "TypeError" "The \"string-len func\" expects one string"))))

    (define (string-nth-func interp)
      (let* ((i (stack-pop! interp))
             (str (stack-pop! interp)))
        (if (or
             (not (and (string? str) (number? i)))
             (not (integer? i))
             (< i 0))
            (interp-error! interp "TypeError" "The string-nth func expects one string and one positive integer")
            (begin
              (if (<= (string-length str) i)
                  (interp-error! interp "IndexError" "string index out of range")
                  (stack-push! interp (string (string-ref str (exact i)))))))))


    (define string-func-dict
      `(("string-concat" . ,string-concat-func)
        ("string-len" . ,string-len-func)
        ("string-nth" . ,string-nth-func)))))
