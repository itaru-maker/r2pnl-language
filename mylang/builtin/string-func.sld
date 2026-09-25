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

    (define (string-length-func interp)
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

    (define (substring-func interp)
      (let* ((end (stack-pop! interp))
             (start (stack-pop! interp))
             (str (stack-pop! interp)))
        (cond
         ((not
           (and
            (number? end)
            (number? start)
            (integer? end)
            (integer? start)
            (positive? end)
            (positive? start)
            (string? str)))
          (interp-error! interp "TypeError" "the \"substring\" func expects a string and two positive integer"))

         ((< end start)
          (interp-error! interp "IndexError" "The start value must be greater than the end value"))
         ((<= (string-length str) end)
          (interp-error! interp "IndexError" "string index out of range"))
         (else
          (stack-push! interp (substring str start end))))))


    (define string-func-dict
      `(("string-concat" . ,string-concat-func)
        ("string-length" . ,string-length-func)
        ("string-nth" . ,string-nth-func)
        ("substring" . ,substring-func)))))
