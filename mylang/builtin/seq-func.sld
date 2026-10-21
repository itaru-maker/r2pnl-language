(define-library (mylang builtin seq-func)
  (export seq-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang tokens)
          (mylang error)
          (mylang interpreter))
  (begin
    (define (empty?-func interp)
      (let* ((v (stack-pop! interp)))
        (if (not (or (string? v) (block-value? v)))
            (interp-error! interp
                           "TypeError"
                           (string-append "the \"empty?\" func expects a string or a block, but got "
                                          (value->write-string v)))
            (stack-push! interp
                         (or (and (string? v) (equal? v ""))
                             (and (block-value? v) (null? (block-value-items v))))))))
  
    (define (concat-func interp)
      (let* ((b (stack-pop! interp))
             (a (stack-pop! interp)))
        (cond
         ((and (string? a) (string? b))
          (stack-push! interp (string-append a b)))
         ((and (block-value? a) (block-value? b))
          (stack-push! interp (make-block-value (append (block-value-items a) (block-value-items b)))))
         (else
          (interp-error! interp
                         "TypeError"
                         (string-append
                          "the \"concat\" func expects either two strings or two blocks, but got "
                          (value->write-string b)
                          " and "
                          (value->write-string a)))))))

    (define (length-func interp)
      (let* ((v (stack-pop! interp)))
        (cond
         ((string? v)
          (stack-push! interp (string-length v)))
         ((block-value? v)
          (stack-push! interp (length (block-value-items v))))
         (else
          (interp-error! interp
                         "TypeError"
                         (string-append
                          "the \"length\" func expects a string or a block, but got "
                          (value->write-string v)))))))

    (define (nth-func interp)
      (let* ((i (stack-pop! interp))
             (seq (stack-pop! interp)))
        (cond
         ((or (not (integer? i)) (< i 0))
          (interp-error! interp
                         "TypeError"
                         (string-append "the \"nth\" func expects a non-negative integer as index, but got "
                                        (value->write-string i))))
         ((string? seq)
          (if (>= i (string-length seq))
              (interp-error! interp "IndexError" "string index out of range")
              (stack-push! interp (string (string-ref seq (exact i))))))

         ((block-value? seq)
          (if (>= i (length (block-value-items seq)))
              (interp-error! interp "IndexError" "block index out of range")
              (stack-push! interp (car (list-ref (block-value-items seq) (exact i))))));行番号のcdrは捨てる
         (else
          (interp-error! interp
                         "TypeError"
                         (string-append "the \"nth\" func expects a string or a block as sequence, but got "
                                        (value->write-string seq)))))))
    
    (define seq-func-dict
      `(("empty?" . ,empty?-func)
        ("concat" . ,concat-func)
        ("length" . ,length-func)
        ("nth" . ,nth-func)))))
