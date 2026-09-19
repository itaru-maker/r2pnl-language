(define-library (mylang builtin logic-func)
  (export logic-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang interpreter))
  (begin
    (define (branch value)
      (not (or (eq? value #f) (eq? value the-nil))))
    
    (define (and-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (stack-push! interp (and (branch a) (branch b)))))

    (define (or-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (stack-push! interp (or (branch a) (branch b)))))

    (define (not-func interp)
      (let* ((a (stack-pop! interp)))
        (stack-push! interp (not (branch a)))))

    (define (xor-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (stack-push! interp (if (branch a)
                               (not (branch b))
                               (branch b)))))

    (define logic-func-dict
      `(("and" . ,and-func)
        ("or" . ,or-func)
        ("not" . ,not-func)
        ("xor" . ,xor-func)))))
