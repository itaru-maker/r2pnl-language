;;廃止するかも？
(define-library (mylang builtin string-func)
  (export string-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang interpreter))

  (begin
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
            (<= 0 end)
            (<= 0 start)
            (string? str)))
          (interp-error! interp "TypeError" "the \"substring\" func expects a string and two positive integer"))

         ((< end start)
          (interp-error! interp "IndexError" "The end value must be greater than or equal to the start value"))
         ((< (string-length str) end)
          (interp-error! interp "IndexError" "string index out of range"))
         (else
          (stack-push! interp (substring str (exact start) (exact end)))))))


    (define string-func-dict
      `(("substring" . ,substring-func)))))
